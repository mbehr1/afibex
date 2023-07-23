// copyright Matthias Behr, (c) 2023
//
// parser for franca_json files as fibex data
// todos:
// SomeIpEnumBitWidth, SomeIpEnumWidth, SomeIpEnumInvalidValue,SomeIpStructLengthWidth,StructBitField,...

use std::{
    collections::HashMap, error::Error, fs::File, io::BufReader, path::Path, sync::Arc,
    time::Instant,
};

use serde_json::Value;

use crate::fibex::{
    ArrayDimension, BaseDataType, CodedType, Coding, ComplexDatatype, ComplexDatatypeClass,
    Datatype, DatatypeType, Encoding, Enum, FibexData, FibexError, GetterSetterNotifier, Method,
    MethodIdType, Parameter, SerializationAttributes, Service, Utilization,
};

impl FibexData {
    pub fn parse_franca_json(&mut self, file_path: &Path) -> Result<(), Box<dyn Error>> {
        println!("parse_franca_json({:?})...", file_path);
        let start = Instant::now();
        let file = File::open(file_path)?;
        let reader = BufReader::new(file);
        let fj: Value = serde_json::from_reader(reader)?;
        let duration = start.elapsed();
        println!(
            "parse_franca_json({:?})... parsed as json taking {:?}",
            file_path, duration
        );
        match fj {
            Value::Object(fj) => {
                println!(
                    "parse_franca_json: got obj with keys:{:?}",
                    fj.keys().collect::<Vec<&String>>()
                );

                // insert some basic codings
                // todo insert which ones? only from EnumBackingTypes?
                // and Boolean, Float, Double, String?
                self.insert_basic_types();

                // parse datatypes first (so interfaces could verify existance already)
                if let Some(fj) = fj.get("datatypes").and_then(|f| f.as_object()) {
                    /*println!(
                        "parse_franca_json: got datatypes {:?}",
                        fj.keys().collect::<Vec<_>>()
                    );*/
                    for (datatype_name, val) in fj.iter() {
                        let dt = self.parse_fj_datatype(datatype_name, val)?;
                        self.elements
                            .datatypes_map_by_id
                            .insert(dt.id.to_owned(), dt);
                    }
                }

                if let Some(fs) = fj.get("interfaces").and_then(|f| f.as_object()) {
                    /*println!(
                        "parse_franca_json: got interfaces {:?}",
                        fs.keys().collect::<Vec<_>>()
                    );*/
                    for (intf, val) in fs.iter() {
                        // we do map all interfaces with deployment.network.someip to a Service
                        self.parse_fj_interface(intf, val)?;
                    }
                }

                // perform some checks:
                let _ = self.validate_datatypes();
            }
            _ => {
                println!(
                "parse_franca_json: unexpected value of root object. Treating as no franca_json!",
            );
                return Err(FibexError::new("expecting only object").into());
            }
        }
        let duration = start.elapsed();
        println!(
            "parse_franca_json({:?})... parsed as fibex taking total {:?}",
            file_path, duration
        );
        Ok(())
    }

    pub fn validate_datatypes(&self) -> Result<(), FibexError> {
        let mut nr_errs = 0u32;
        for (dt_name, dt) in &self.elements.datatypes_map_by_id {
            match &dt.datatype {
                DatatypeType::ComplexType(cdt) => {
                    for member in &cdt.members {
                        if !self
                            .elements
                            .datatypes_map_by_id
                            .contains_key(&member.datatype_ref)
                        {
                            nr_errs += 1;
                            println!(
                                "validate_datatypes(cdt {}): unknown datatype {}!",
                                dt_name, member.datatype_ref
                            );
                        }
                    }
                }
                DatatypeType::EnumType { coding_ref, .. } => {
                    if !self.pi.codings.contains_key(coding_ref) {
                        nr_errs += 1;
                        println!(
                            "validate_datatypes(enum {}): unknown coding {}!",
                            dt_name, coding_ref
                        );
                    }
                }
                DatatypeType::Common(coding) => {
                    if !self.pi.codings.contains_key(coding) {
                        nr_errs += 1;
                        println!(
                            "validate_datatypes(common {}): unknown coding {}!",
                            dt_name, coding
                        );
                    }
                }
            }
        }
        if nr_errs == 0 {
            Ok(())
        } else {
            Err(FibexError {
                msg: format!("got {} errors from validation!", nr_errs),
            })
        }
    }

    fn parse_fj_datatype(
        &mut self,
        datatype_name: &String,
        val: &Value,
    ) -> Result<Datatype, Box<dyn Error>> {
        if let Some(val) = val.as_object() {
            let data_type = get_mandatory_string(val, &["type"])?;
            let short_name = get_string(val, &["name"]).map(|v| v.to_owned());
            let desc = get_string(val, &["tags", "description"]).map(|v| v.to_owned());
            let datatype = match data_type {
                "Struct" | "Union" => {
                    let is_union = data_type.eq("Union");
                    let items = get_child(val, &["items"]).and_then(|v| v.as_array());
                    if let Some(items) = items {
                        let members = items
                            .iter()
                            .enumerate()
                            .filter_map(|(idx, item)| {
                                let item = item.as_object();
                                let mut p: Option<Parameter> = None;
                                if let Some(item) = item {

                                    p = Self::parse_fj_parameter(b'A', format!("{}_{}", datatype_name, idx).as_str(), item, idx as i32, None); // todo parent_endianess?
                                }
                                if p.is_none() {
                                    println!(
                                        "ignored item {}.items[{}] due to missing/wrong attributes:{:?}",
                                        datatype_name, idx, item
                                    );
                                }
                                p
                            })
                            .collect();
                        DatatypeType::ComplexType(ComplexDatatype {
                            class: if is_union {
                                ComplexDatatypeClass::Union
                            } else {
                                ComplexDatatypeClass::Structure
                            },
                            members,
                        })
                    } else {
                        return Err(
                            FibexError::new(&format!("no items array on '{}'", data_type)).into(),
                        );
                    }
                }
                "Enumeration" => {
                    let items = get_child(val, &["items"]).and_then(|v| v.as_array());
                    if let Some(items) = items {
                        let enums = items
                            .iter()
                            .enumerate()
                            .filter_map(|(idx, item)| {
                                let item = item.as_object();
                                let mut p: Option<Enum> = None;
                                if let Some(item) = item {
                                    let desc = get_string(item, &["tags", "description"]).map(|v| v.to_owned());
                                    let name =
                                        get_string(item, &["name"]).map(|v| v.to_owned());
                                    let value = get_child(item, &["value"]).and_then(|v|v.as_i64());
                                    if let Some(value) = value {
                                            p = Some(Enum {
                                                value:value.into(),
                                                synonym: name,
                                                desc
                                            });
                                    }
                                }
                                if p.is_none() {
                                    println!(
                                        "ignored item {}.items[{}] due to missing/wrong attributes:{:?}",
                                        datatype_name, idx, item
                                    );
                                }
                                p
                            })
                            .collect();

                        let depl = get_child(val, &["deployment", "network.someip"]);
                        if let Some(depl) = depl.and_then(|v| v.as_object()) {
                            if depl.keys().len() != 1 {
                                println!(
                                    "parse_fj_datatype: ignoring datatype {} due to !=1 deployment/network.someip",
                                    datatype_name
                                );
                                return Err(FibexError::new(&format!(
                                    "ignoring datatype {} due to !=1 deployment/network.someip",
                                    datatype_name
                                ))
                                .into());
                            } else {
                                let depl_nwsip =
                                    depl.values()
                                        .next()
                                        .and_then(|v| v.as_object())
                                        .ok_or(FibexError::new("network.someip type error"))?;
                                let enum_backing_type =
                                    get_string(depl_nwsip, &["EnumBackingType"]);
                                // todo parse SomeIpEnumWidth
                                if let Some(enum_backing_type) = enum_backing_type {
                                    DatatypeType::EnumType {
                                        coding_ref: enum_backing_type.to_owned(),
                                        enums,
                                    }
                                } else {
                                    return Err(FibexError::new(&format!(
                                        "ignoring datatype {} due to no EnumBackingType",
                                        datatype_name
                                    ))
                                    .into());
                                }
                            }
                        } else {
                            return Err(FibexError::new(&format!(
                                "ignoring datatype {} due to no deployment/network.someip",
                                datatype_name
                            ))
                            .into());
                        }
                    } else {
                        return Err(
                            FibexError::new(&format!("no items array on '{}'", data_type)).into(),
                        );
                    }
                }
                "TypeDef" => {
                    let datatype_ref = get_string(val, &["datatype"]);
                    // todo are there TypeDef with deployment or array infos?
                    if let Some(datatype_ref) = datatype_ref {
                        DatatypeType::ComplexType(ComplexDatatype {
                            class: ComplexDatatypeClass::Typedef,
                            members: vec![Parameter {
                                id: datatype_name.to_owned(),
                                short_name: None,
                                desc: None,
                                datatype_ref: datatype_ref.to_owned(),
                                position: 0,
                                mandatory: true,
                                array_dimensions: vec![],
                                utilization: None,
                                getter: None,
                                setter: None,
                                notifier: None,
                            }],
                        })
                    } else {
                        return Err(FibexError::new(&format!(
                            "no datatype on TypeDef '{}'",
                            datatype_name
                        ))
                        .into());
                    }
                }
                "Array" => {
                    // todo refactor using Self::parse_fj_parameter... to avoid code duplication

                    // datatype, array_size [min, max]
                    // deployment network.someip SomeIpArrayLengthWidth, ...MaxLength, MinLength...
                    let datatype_ref = get_string(val, &["datatype"]);
                    if let Some(datatype_ref) = datatype_ref {
                        let array_size = get_child(val, &["array_size"])
                            .and_then(|v| v.as_array())
                            .filter(|v| v.len() == 2)
                            .map(|v| {
                                v.iter()
                                    .map(|v| {
                                        u32::try_from(v.as_u64().unwrap_or_default())
                                            .unwrap_or_default()
                                    })
                                    .collect::<Vec<_>>()
                            });
                        if let Some(array_size) = array_size {
                            let mut serialization_attributes = None;
                            let depl_o = get_single_object(val, &["deployment", "network.someip"]);
                            if let Some((_, o)) = depl_o {
                                let sip_array_len_width = get_child(o, &["SomeIpArrayLengthWidth"])
                                    .and_then(|v| v.as_u64());
                                serialization_attributes =
                                    sip_array_len_width.map(|sip_array_len_width| {
                                        SerializationAttributes {
                                            array_length_field_size: u32::try_from(
                                                sip_array_len_width * 8,
                                            )
                                            .ok(),
                                            length_field_size: None,
                                            type_field_size: None,
                                            bit_alignment: None,
                                            pass_on_to_subelements: None,
                                        }
                                    });

                                let sip_array_max_len = get_child(o, &["SomeIpArrayMaxLength"])
                                    .and_then(|v| v.as_u64());
                                let sip_array_min_len = get_child(o, &["SomeIpArrayMinLength"])
                                    .and_then(|v| v.as_u64());
                                if let Some(sip_array_min_len) = sip_array_min_len {
                                    if sip_array_min_len != array_size[0] as u64 {
                                        return Err(FibexError::new(&format!(
                                            "array_size mismatch array_min {} vs {} on Array '{}'",
                                            sip_array_min_len, array_size[0], datatype_name
                                        ))
                                        .into());
                                    }
                                }

                                if let Some(sip_array_max_len) = sip_array_max_len {
                                    if sip_array_max_len != array_size[1] as u64 {
                                        return Err(FibexError::new(&format!(
                                            "array_size mismatch array_max {} vs {} on Array '{}'",
                                            sip_array_max_len, array_size[1], datatype_name
                                        ))
                                        .into());
                                    }
                                }
                            }

                            let utilization = if serialization_attributes.is_some() {
                                Some(Utilization {
                                    coding_ref: None,
                                    bit_length: None,
                                    min_bit_length: None,
                                    max_bit_length: None,
                                    is_high_low_byte_order: None,
                                    serialization_attributes,
                                })
                            } else {
                                None
                            };

                            DatatypeType::ComplexType(ComplexDatatype {
                                class: ComplexDatatypeClass::Typedef,
                                members: vec![Parameter {
                                    id: datatype_name.to_owned(),
                                    short_name: None,
                                    desc: None,
                                    datatype_ref: datatype_ref.to_owned(),
                                    position: 0,
                                    mandatory: true,
                                    array_dimensions: vec![ArrayDimension {
                                        dimension: 1,
                                        minimum_size: Some(array_size[0]),
                                        maximum_size: Some(array_size[1]),
                                        bit_alignment: None,
                                    }],
                                    utilization,
                                    getter: None,
                                    setter: None,
                                    notifier: None,
                                }],
                            })
                        } else {
                            return Err(FibexError::new(&format!(
                                "wrong array_size on Array '{}'",
                                datatype_name
                            ))
                            .into());
                        }
                    } else {
                        return Err(FibexError::new(&format!(
                            "no datatype on Array '{}'",
                            datatype_name
                        ))
                        .into());
                    }
                }
                /*
                "Map" => {

                }*/
                _ => {
                    return Err(
                        FibexError::new(&format!("not supported type '{}'", data_type)).into(),
                    )
                }
            };
            let d = Datatype {
                id: datatype_name.to_owned(),
                short_name,
                desc,
                datatype,
            };
            Ok(d)
        } else {
            println!(
                "parse_fj_datatype({}): unexpected value {:?} (no object)!",
                datatype_name, val
            );
            Err(FibexError::new(&format!(
                "parse_fj_datatype({}): unexpected value {:?} (no object)!",
                datatype_name, val
            ))
            .into())
        }
    }

    fn parse_fj_interface(&mut self, intf: &String, val: &Value) -> Result<(), Box<dyn Error>> {
        if let Some(val) = val.as_object() {
            let id = intf;
            let short_name = get_string(val, &["name"]);
            let desc = get_string(val, &["tags", "description"]);
            let depl = get_single_object(val, &["deployment", "network.someip"]);
            if let Some((_depl_name, depl)) = depl {
                let service_identifier: Option<u16> = get_child(depl, &["SomeIpServiceID"])
                    .and_then(|v| v.as_u64())
                    .and_then(|v| u16::try_from(v).ok());

                if let Some(api_version) = get_child(val, &["version"]).and_then(|v| v.as_object())
                {
                    let major = get_child(api_version, &["major"]).and_then(|v| v.as_u64());
                    let minor = get_child(api_version, &["minor"]).and_then(|v| v.as_u64());

                    // parse attributes as methods_by_mid and fields
                    let mut fields = Vec::with_capacity(16);
                    let mut methods_by_mid = HashMap::with_capacity(16);

                    self.parse_fj_interface_attributes(
                        intf,
                        val,
                        &mut fields,
                        &mut methods_by_mid,
                    )?;

                    self.parse_fj_interface_methods(intf, val, &mut methods_by_mid)?;

                    self.parse_fj_interface_broadcasts(intf, val, &mut methods_by_mid)?;

                    let service = Service {
                        id: id.to_owned(),
                        short_name: short_name.map(|f| f.to_owned()),
                        desc: desc.map(|f| f.to_owned()),
                        service_identifier,
                        api_version: (
                            u8::try_from(major.unwrap_or_default()).unwrap_or_default(),
                            u8::try_from(minor.unwrap_or_default()).unwrap_or_default(),
                        ),
                        fields,
                        methods_by_mid,
                    };
                    self.elements
                        .services_map_by_sid_major
                        .entry((
                            service.service_identifier.unwrap_or_default(),
                            service.api_version.0,
                        ))
                        .or_default()
                        .push(service);
                } else {
                    println!(
                        "parse_fj_interface: ignoring interface {} due to no version",
                        intf
                    );
                }
            } else {
                // we skip
                println!(
                    "parse_fj_interface: ignoring interface {} due to no deployment/network.someip",
                    intf
                );
            }
        } else {
            println!(
                "parse_fj_interface: unexpected value {:?} (no object)!",
                val
            );
        }
        Ok(())
    }

    fn parse_fj_parameter(
        param_typ: u8,
        id: &str,
        val: &serde_json::Map<String, Value>,
        position: i32,
        parent_endianess: Option<&str>,
    ) -> Option<Parameter> {
        let lookup_names = |x: u8| -> (&'static str, &'static str) {
            match x {
                b'M' => ("SomeIpMethodEndianess", "SomeIpNotifierID"), // notifier not needed here
                b'B' => ("SomeIpBroadcastEndianess", "SomeIpEventID"),
                _ => ("SomeIpAttributeEndianess", "SomeIpNotifierID"),
            }
        };

        let short_name = get_string(val, &["name"]).map(|v| v.to_owned());
        let desc = get_string(val, &["tags", "description"]).map(|v| v.to_owned());
        let datatype_ref = get_string(val, &["datatype"])?;

        let array_size = get_child(val, &["array_size"])
            .and_then(|v| v.as_array())
            .filter(|v| v.len() == 2)
            .map(|v| {
                v.iter()
                    .map(|v| u32::try_from(v.as_u64().unwrap_or_default()).unwrap_or_default())
            });

        let array_dimensions = if let Some(mut array_size) = array_size {
            vec![ArrayDimension {
                dimension: 1,
                minimum_size: array_size.next(),
                maximum_size: array_size.next(),
                bit_alignment: None,
            }]
        } else {
            vec![]
        };

        // we can have simple parameters without deployment/network.someip. (but not arrays)

        let depl = get_single_object(val, &["deployment", "network.someip"]);
        let sd = if let Some((_depl_name, depl_nwsip)) = depl {
            let mut serialization_attributes = None;
            let bit_length = get_child(depl_nwsip, &["SomeIpIntegerBitWidth"])
                .and_then(|v| v.as_u64())
                .and_then(|v| u32::try_from(v).ok());

            let endianess =
                get_string(depl_nwsip, &[lookup_names(param_typ).0]).or(parent_endianess);
            let getter = get_child(depl_nwsip, &["SomeIpGetterID"])
                .and_then(|v| v.as_u64())
                .and_then(|v| u16::try_from(v).ok())
                .map(|v| GetterSetterNotifier {
                    method_identifier: v,
                });
            let setter = get_child(depl_nwsip, &["SomeIpSetterID"])
                .and_then(|v| v.as_u64())
                .and_then(|v| u16::try_from(v).ok())
                .map(|v| GetterSetterNotifier {
                    method_identifier: v,
                });
            let notifier = get_child(depl_nwsip, &[lookup_names(param_typ).1])
                .and_then(|v| v.as_u64())
                .and_then(|v| u16::try_from(v).ok())
                .map(|v| GetterSetterNotifier {
                    method_identifier: v,
                });

            if !array_dimensions.is_empty() {
                let sip_array_len_width =
                    get_child(depl_nwsip, &["SomeIpArrayLengthWidth"]).and_then(|v| v.as_u64());
                serialization_attributes =
                    sip_array_len_width.map(|sip_array_len_width| SerializationAttributes {
                        array_length_field_size: u32::try_from(sip_array_len_width * 8).ok(),
                        length_field_size: None,
                        type_field_size: None,
                        bit_alignment: None,
                        pass_on_to_subelements: None,
                    });

                let sip_array_max_len =
                    get_child(depl_nwsip, &["SomeIpArrayMaxLength"]).and_then(|v| v.as_u64());
                let sip_array_min_len =
                    get_child(depl_nwsip, &["SomeIpArrayMinLength"]).and_then(|v| v.as_u64());
                if let Some(sip_array_min_len) = sip_array_min_len {
                    if let Some(min_size) = array_dimensions[0].minimum_size {
                        if sip_array_min_len != min_size as u64 {
                            println!(
                                "parameter id {} has mismatching SomeIpArrayMinLength! {:?}",
                                id, depl_nwsip
                            );
                        }
                    }
                }

                if let Some(sip_array_max_len) = sip_array_max_len {
                    if let Some(max_size) = array_dimensions[0].maximum_size {
                        if sip_array_max_len != max_size as u64 {
                            println!(
                                "parameter id {} has mismatching SomeIpArrayMinLength! {:?}",
                                id, depl_nwsip
                            );
                        }
                    }
                }
            }

            let utilization = if endianess.is_some()
                || serialization_attributes.is_some()
                || bit_length.is_some()
            {
                Some(Utilization {
                    coding_ref: None,
                    bit_length,
                    min_bit_length: None,
                    max_bit_length: None,
                    is_high_low_byte_order: endianess
                        .map(|endianess| endianess.eq_ignore_ascii_case("be")),
                    serialization_attributes,
                })
            } else {
                None
            };
            Some((getter, setter, notifier, utilization))
        } else {
            None
        };

        if let Some(sd) = sd {
            Some(Parameter {
                id: id.to_owned(), // or the deployment/network.someip name?
                position,
                short_name,
                desc,
                datatype_ref: datatype_ref.to_owned(),
                mandatory: false,
                array_dimensions,
                utilization: sd.3,
                getter: sd.0,
                setter: sd.1,
                notifier: sd.2,
            })
        } else {
            let utilization = if parent_endianess.is_some() {
                Some(Utilization {
                    coding_ref: None,
                    bit_length: None,
                    min_bit_length: None,
                    max_bit_length: None,
                    is_high_low_byte_order: parent_endianess
                        .map(|endianess| endianess.eq_ignore_ascii_case("be")),
                    serialization_attributes: None,
                })
            } else {
                None
            };
            Some(Parameter {
                id: id.to_owned(), // or the deployment/network.someip name?
                position,
                short_name,
                desc,
                datatype_ref: datatype_ref.to_owned(),
                mandatory: false,
                array_dimensions,
                utilization,
                getter: None,
                setter: None,
                notifier: None,
            })
        }
    }

    fn parse_fj_interface_attributes(
        &self,
        intf: &String,
        val: &serde_json::Map<String, Value>,
        fields: &mut Vec<Arc<Parameter>>,
        methods_by_mid: &mut HashMap<u16, MethodIdType>,
    ) -> Result<(), Box<dyn Error>> {
        let attributes = get_child(val, &["attributes"]).and_then(|v| v.as_object());
        if let Some(attributes) = attributes {
            for (attr_name, val) in attributes.iter() {
                if let Some(val) = val.as_object() {
                    let p = Self::parse_fj_parameter(b'A', attr_name, val, 0, None);
                    if let Some(p) = p {
                        let field: Arc<Parameter> = Arc::new(p);
                        if let Some(getter) = &field.getter {
                            methods_by_mid.insert(
                                getter.method_identifier,
                                MethodIdType::Getter {
                                    field: field.clone(),
                                },
                            );
                        }
                        if let Some(setter) = &field.setter {
                            methods_by_mid.insert(
                                setter.method_identifier,
                                MethodIdType::Setter {
                                    field: field.clone(),
                                },
                            );
                        }
                        if let Some(notif) = &field.notifier {
                            methods_by_mid.insert(
                                notif.method_identifier,
                                MethodIdType::Notifier {
                                    field: field.clone(),
                                },
                            );
                        }
                        fields.push(field)
                    }
                }
            }
        } else if !(val.contains_key("broadcasts") || val.contains_key("methods")) {
            println!(
                "Interface {} has no attributes/methods/broadcasts. keys={:?}",
                intf,
                val.keys().map(|k| k.as_str()).collect::<Vec<_>>()
            )
        }
        Ok(())
    }

    fn parse_fj_interface_broadcasts(
        &self,
        intf: &String,
        val: &serde_json::Map<String, Value>,
        methods_by_mid: &mut HashMap<u16, MethodIdType>,
    ) -> Result<(), Box<dyn Error>> {
        // map all broadcasts as Event(Method)
        let broadcasts = get_child(val, &["broadcasts"]).and_then(|v| v.as_object());
        if let Some(broadcasts) = broadcasts {
            for (broadcast_name, val) in broadcasts.iter() {
                if let Some(val) = val.as_object() {
                    let short_name = get_string(val, &["name"]).map(|v| v.to_owned());
                    let desc = get_string(val, &["tags", "description"]).map(|v| v.to_owned());
                    let sd = get_single_object(val, &["deployment", "network.someip"]);
                    let (id, endianess) = if let Some((_sd_name, sd)) = sd {
                        let id = get_child(sd, &["SomeIpEventID"])
                            .and_then(|v| v.as_u64())
                            .and_then(|v| u16::try_from(v).ok());
                        let endianess = get_string(sd, &["SomeIpBroadcastEndianess"]);
                        (id, endianess)
                    } else {
                        (None, None)
                    };

                    // inputs and outputs parameter
                    let input_params = vec![];
                    let mut return_params = vec![];

                    let inputs = get_child(val, &["inputs"]).and_then(|v| v.as_array());
                    if let Some(inputs) = inputs {
                        if !inputs.is_empty() {
                            println!(
                                "Interface {} ignored inputs for broadcast '{}' as unexpected!",
                                intf, broadcast_name
                            )
                        }
                    }
                    let outputs = get_child(val, &["outputs"]).and_then(|v| v.as_array());
                    if let Some(outputs) = outputs {
                        for (idx, output) in outputs.iter().enumerate() {
                            if let Some(output) = output.as_object() {
                                let p = Self::parse_fj_parameter(
                                    b'M',
                                    format!("{}_outputs_{}", broadcast_name, idx).as_str(),
                                    output,
                                    idx as i32,
                                    endianess,
                                );
                                if let Some(p) = p {
                                    return_params.push(p);
                                }
                            }
                        }
                    }

                    if let Some(id) = id {
                        methods_by_mid.insert(
                            id,
                            MethodIdType::Method(Method {
                                id: broadcast_name.to_owned(), // or sd_name?
                                short_name,
                                desc,
                                method_identifier: Some(id),
                                input_params,
                                return_params,
                            }),
                        );
                    } else {
                        println!(
                            "Interface {} ignored broadcast '{}' due to missing SomeIpEventID",
                            intf, broadcast_name
                        )
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_fj_interface_methods(
        &self,
        intf: &String,
        val: &serde_json::Map<String, Value>,
        methods_by_mid: &mut HashMap<u16, MethodIdType>,
    ) -> Result<(), Box<dyn Error>> {
        let methods = get_child(val, &["methods"]).and_then(|v| v.as_object());
        if let Some(methods) = methods {
            for (method_name, val) in methods.iter() {
                if let Some(val) = val.as_object() {
                    let short_name = get_string(val, &["name"]).map(|v| v.to_owned());
                    let desc = get_string(val, &["tags", "description"]).map(|v| v.to_owned());
                    let sd = get_single_object(val, &["deployment", "network.someip"]);
                    let (id, endianess) = if let Some((_sd_name, sd)) = sd {
                        let id = get_child(sd, &["SomeIpMethodID"])
                            .and_then(|v| v.as_u64())
                            .and_then(|v| u16::try_from(v).ok());
                        let endianess = get_string(sd, &["SomeIpMethodEndianess"]);
                        (id, endianess)
                    } else {
                        (None, None)
                    };

                    // inputs and outputs parameter
                    let mut input_params = vec![];
                    let mut return_params = vec![];

                    let inputs = get_child(val, &["inputs"]).and_then(|v| v.as_array());
                    if let Some(inputs) = inputs {
                        for (idx, input) in inputs.iter().enumerate() {
                            if let Some(input) = input.as_object() {
                                let p = Self::parse_fj_parameter(
                                    b'M',
                                    format!("{}_inputs_{}", method_name, idx).as_str(),
                                    input,
                                    idx as i32,
                                    endianess,
                                );
                                if let Some(p) = p {
                                    input_params.push(p);
                                }
                            }
                        }
                    }
                    let outputs = get_child(val, &["outputs"]).and_then(|v| v.as_array());
                    if let Some(outputs) = outputs {
                        for (idx, output) in outputs.iter().enumerate() {
                            if let Some(output) = output.as_object() {
                                let p = Self::parse_fj_parameter(
                                    b'M',
                                    format!("{}_outputs_{}", method_name, idx).as_str(),
                                    output,
                                    idx as i32,
                                    endianess,
                                );
                                if let Some(p) = p {
                                    return_params.push(p);
                                }
                            }
                        }
                    }

                    if let Some(id) = id {
                        methods_by_mid.insert(
                            id,
                            MethodIdType::Method(Method {
                                id: method_name.to_owned(), // or sd_name?
                                short_name,
                                desc,
                                method_identifier: Some(id),
                                input_params,
                                return_params,
                            }),
                        );
                    } else {
                        println!(
                            "Interface {} ignored method '{}' due to missing SomeIpMethodID",
                            intf, method_name
                        )
                    }
                }
            }
        }
        Ok(())
    }

    fn insert_basic_types(&mut self) {
        let mut insert_coding_and_dt =
            |name: &str, bit_length: u32, base_data_type: Option<BaseDataType>| {
                self.pi.codings.insert(
                    name.to_owned(),
                    Coding {
                        id: name.to_owned(),
                        short_name: None,
                        coded_type: Some(CodedType {
                            bit_length: Some(bit_length),
                            min_length: None,
                            max_length: None,
                            base_data_type,
                            category: crate::fibex::Category::StandardLengthType,
                            encoding: None,
                            termination: None,
                        }),
                        compu_methods: vec![],
                    },
                );
                self.elements.datatypes_map_by_id.insert(
                    name.to_owned(),
                    Datatype {
                        id: name.to_owned(),
                        short_name: None,
                        desc: None,
                        datatype: DatatypeType::Common(name.to_owned()),
                    },
                );
            };

        insert_coding_and_dt("UInt8", 8, Some(crate::fibex::BaseDataType::AUint8));
        insert_coding_and_dt("UInt16", 16, Some(crate::fibex::BaseDataType::AUint16));
        insert_coding_and_dt("UInt32", 32, Some(crate::fibex::BaseDataType::AUint32));
        insert_coding_and_dt("UInt64", 64, Some(crate::fibex::BaseDataType::AUint64));
        insert_coding_and_dt("Int8", 8, Some(crate::fibex::BaseDataType::AInt8));
        insert_coding_and_dt("Int16", 16, Some(crate::fibex::BaseDataType::AInt16));
        insert_coding_and_dt("Int32", 32, Some(crate::fibex::BaseDataType::AInt32));
        insert_coding_and_dt("Int64", 64, Some(crate::fibex::BaseDataType::AInt64));
        insert_coding_and_dt("Float", 32, Some(crate::fibex::BaseDataType::AFloat32));
        insert_coding_and_dt("Double", 64, Some(crate::fibex::BaseDataType::AFloat64));

        // Boolean = AUint8
        insert_coding_and_dt("Boolean", 8, Some(crate::fibex::BaseDataType::AUint8));

        // "String" = "STRING_UTF8_DYNAMIC"
        self.pi.codings.insert(
            "String".to_owned(),
            Coding {
                id: "String".to_owned(),
                short_name: None,
                coded_type: Some(CodedType {
                    bit_length: None,
                    min_length: None,
                    max_length: None,
                    base_data_type: Some(crate::fibex::BaseDataType::AUnicode2String),
                    category: crate::fibex::Category::LeadingLengthInfoType,
                    encoding: Some(Encoding::Utf8),
                    termination: Some(crate::fibex::HoTermination::Zero),
                }),
                compu_methods: vec![],
            },
        );
        self.elements.datatypes_map_by_id.insert(
            "String".to_owned(),
            Datatype {
                id: "String".to_owned(),
                short_name: None,
                desc: None,
                datatype: DatatypeType::Common("String".to_owned()),
            },
        );
    }
}

fn get_mandatory_string<'a>(
    o: &'a serde_json::Map<String, Value>,
    keys: &[&str],
) -> Result<&'a str, FibexError> {
    get_child(o, keys)
        .and_then(|v| v.as_str())
        .ok_or_else(|| FibexError::new("mandatory member missing!"))
}

fn get_string<'a>(o: &'a serde_json::Map<String, Value>, keys: &[&str]) -> Option<&'a str> {
    get_child(o, keys).and_then(|v| v.as_str())
}

fn get_child<'a>(o: &'a serde_json::Map<String, Value>, keys: &[&str]) -> Option<&'a Value> {
    match keys.len() {
        0 => None,
        1 => o.get(keys[0]),
        _ => o
            .get(keys[0])
            .and_then(|v| v.as_object())
            .and_then(|v| get_child(v, &keys[1..])),
    }
}
/// return the object referenced by keys if it's the only member.
/// e.g. deployment."network.someip".<single_member>:{...}
///
/// returns pair ("single_member", {...})
fn get_single_object<'a>(
    o: &'a serde_json::Map<String, Value>,
    keys: &[&str],
) -> Option<(&'a str, &'a serde_json::Map<String, Value>)> {
    get_single_value(o, keys).and_then(|(v, o)| o.as_object().map(|o| (v, o)))
}

fn get_single_value<'a>(
    o: &'a serde_json::Map<String, Value>,
    keys: &[&str],
) -> Option<(&'a str, &'a Value)> {
    let c = get_child(o, keys).and_then(|v| v.as_object());
    if let Some(c) = c {
        if c.keys().len() != 1 {
            println!(
                "get_single_value: returning None for {:?} due to keys {}!=1",
                keys,
                c.keys().len()
            );
            None
        } else if let Some((k, v)) = c.iter().next() {
            Some((k.as_str(), v))
        } else {
            None
        }
    } else {
        None
    }
}

pub fn parse_franca_json(file: &Path, fd: &mut FibexData) -> Result<(), Box<dyn Error>> {
    FibexData::parse_franca_json(fd, file)
}
