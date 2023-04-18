// copyright Matthias Behr, (c) 2022
//
// todos:
// [ ] test importing multiple fibex for the same services but with different version (diff major, diff minor only)
// [ ] test importing multiple fibex for the same/similar channels/frames
// [ ] support LONG-NAME: parse as array to lang <ho:LONG-NAME xml:lang="en">ABCDE</ho:LONG-NAME>

use quick_xml::{events::Event, Reader};

use std::{
    collections::HashMap,
    error::Error,
    fmt,
    io::BufRead,
    path::{Path, PathBuf},
    sync::Arc,
};

// todo: think about impl fmt::Display for FibexContext struct
// problems: fmt:Error has no means to carry data/info. just that an error occured.

#[derive(Debug)]
pub struct FibexError {
    pub msg: String,
}

impl FibexError {
    pub fn new(msg: &str) -> Self {
        Self {
            msg: msg.to_string(),
        }
    }
}

impl fmt::Display for FibexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.msg)
    }
}

impl Error for FibexError {}

#[derive(Debug)]
pub struct XmlElement {
    // todo might be optimized with ref/lifetimes instead of copies!
    // buf: Vec<u8>,
    pub name: String,
    attributes: Vec<(String, String)>,
    pub children: Vec<XmlElement>,
    pub text: Option<String>,
}

impl XmlElement {
    pub fn attr(&self, name: &str) -> Option<&(String, String)> {
        self.attributes.iter().find(|p| {
            p.0 == name || (p.0.len() > name.len() && XmlElement::non_dotted_name(&p.0) == name)
        })
    }
    pub fn child_by_name(&self, name: &str) -> Option<&XmlElement> {
        self.children.iter().find(|c| c.name == name)
    }

    /// return the name after the ':' or the full name
    fn non_dotted_name(name: &str) -> &str {
        if let Some(idx) = name.rfind(':') {
            if idx < name.len() {
                return &name[idx + 1..];
            }
        }
        name
    }
}

fn read_element<T: BufRead>(
    start_e: &quick_xml::events::BytesStart,
    reader: &mut Reader<T>,
    empty_element: bool,
) -> Result<XmlElement, Box<dyn Error>> {
    let mut xml_e = XmlElement {
        // buf: Vec::new(), if we ever want to return Cow... references into
        name: String::from_utf8(start_e.local_name().to_vec())?,
        attributes: vec![],
        children: vec![],
        text: None,
    };

    let attrs = start_e.attributes().flatten().map(|attribute| {
        (
            String::from_utf8(attribute.key.to_vec()),
            String::from_utf8(attribute.value.to_vec()),
        )
    });
    for attr in attrs {
        if attr.0.is_ok() && attr.1.is_ok() {
            xml_e.attributes.push((attr.0.unwrap(), attr.1.unwrap()));
        } else {
            println!("read_element: wrong attributes for '{}'", xml_e.name);
        }
    }

    let mut buf = Vec::with_capacity(1024); // todo better default?
    if !empty_element {
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => xml_e.children.push(read_element(e, reader, false)?),
                Event::Empty(ref e) => xml_e.children.push(read_element(e, reader, true)?),
                Event::Text(ref e) => {
                    // this text gets updated on each text (incl. whitespace) within the start/end tag
                    let mut text = e.unescape_and_decode(reader)?;
                    if let Some(cur_text) = xml_e.text {
                        text = cur_text + &text;
                    }
                    xml_e.text = Some(text);
                }
                Event::End(ref e) if e.local_name() == start_e.local_name() => break,
                _ => {}
            }
        }
    }

    Ok(xml_e)
}

fn skip_element<T: BufRead>(
    start_e: &quick_xml::events::BytesStart,
    reader: &mut Reader<T>,
) -> Result<(), Box<dyn Error>> {
    let mut nesting_level = 0u32;
    let mut buf = Vec::with_capacity(1024); // better default value?

    loop {
        match reader.read_event(&mut buf)? {
            Event::Start(ref _e) => nesting_level += 1,
            // can be ignored Event::Empty(ref e) => skip_element(e, reader, true)?,
            Event::End(ref e) => {
                if e.local_name() == start_e.local_name() && nesting_level == 0 {
                    break;
                } else if nesting_level > 0 {
                    nesting_level -= 1;
                } else {
                    return Err(quick_xml::Error::EndEventMismatch {
                        found: String::from_utf8(e.local_name().to_vec()).unwrap_or_default(),
                        expected: "unknown".to_string(),
                    }
                    .into());
                };
            }
            _ => {}
        }
    }
    Ok(())
}

/// these structs should reflect the structure from the asam fibex 4.2.1 definition
/// focussed on the needed elements for someip or dlt usage.
/// todo check mandatory (non Option<>) vs use="required"/minOccurrence=0...

#[derive(Debug)]
pub struct Project {
    pub id: String,
    pub short_name: Option<String>,
}

/// fx:CHANNEL-TYPE according to https://www.asam.net/xml/fbx/fibex.xsd
/// It's a fx:REVISED-ELEMENT-TYPE extended by:
///  - opt fx:PDU-TRIGGERINGS
///  - opt fx:FRAME-TRIGGERINGS
///  - opt fx:MANUFACTURER-ECU-EXTENSION
///
#[derive(Debug)]
pub struct Channel {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    // frame_triggerings todo, lets store the identifier->frame-ref mapping only for now for u32 based identifiers
    pub frame_ref_by_frame_triggering_identifier: HashMap<u32, String>,
    pub manufacturer_extension: Option<XmlElement>,
}

/// fx:ECU-TYPE according to https://www.asam.net/xml/fbx/fibex.xsd
/// It's a fx:REVISED-ELEMENT-TYPE extended by:
///  - opt fx:DIAGNOSTIC-ADDRESSES
///  - opt FUNCTION-REFS
///  - opt CONTROLLERS
///  - opt CONNECTORS
///  - opt fx:MANUFACTURER-ECU-EXTENSION
///
#[derive(Debug)]
pub struct Ecu {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub manufacturer_extension: Option<XmlElement>,
}

/// fx:FRAME-TYPE according to https://www.asam.net/xml/fbx/fibex.xsd
/// It's a fx:REVISED-ELEMENT-TYPE extended by:
/// - BYTE-LENGTH: xs:unsignedInt
/// - opt FRAME-TYPE: fx:FRAMETYPE
/// - opt PDU-INSTANCES: fx:PDU-INSTANCES
/// - opt MANUFACTURER-EXTENSION: fx:MANUFACTURER-FRAME-EXTENSION
#[derive(Debug)]
pub struct Frame {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub byte_length: u32,
    pub frame_type: Option<String>,      // todo proper type?
    pub pdu_instances: Vec<PduInstance>, // todo already sorted by sequence-number
    pub manufacturer_extension: Option<XmlElement>,
}

/// fx:SIGNAL-TYPE according to https://www.asam.net/xml/fbx/fibex.xsd
/// It's a fx:REVISED-ELEMENT-TYPE extended by:
/// - opt DEFAULT-VALUE xs:double
/// - CODING-REF
/// - opt SIGNAL-TYPE fx:SIGNAL-TYPE-TYPE
/// - opt PRIORITY xs:positiveInteger
/// - opt ECU-SENDING-BEHAVIORS fx:ECU-SENDING-BEHAVIORS
/// - opt MANUFACTURER-EXTENSION: fx:MANUFACTURER-FRAME-EXTENSION
#[derive(Debug)]
pub struct Signal {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub coding_ref: String,
    pub manufacturer_extension: Option<XmlElement>,
}

/// fx:GENERIC-PDU-INSTANCE-TYPE according to https://www.asam.net/xml/fbx/fibex.xsd
/// It's a fx:IDENTIFIABLE-ELEMENT-TYPE extended by:
/// - PDU-REF: fx:PDU-REF
/// - opt manufacturer_extension: Option<XmlElement>,
#[derive(Debug)]
pub struct PduInstance {
    pub pdu_ref: String,
    // indirectly by Vec order. pub sequence_number: u32,
    pub bit_position: Option<u32>,
    pub is_high_low_byte_order: Option<bool>,
}

/// todo proper type/doc
#[derive(Debug)]
pub struct SignalInstance {
    pub signal_ref: String,
    pub is_high_low_byte_order: Option<bool>,
    // indirectly by Vec order. pub sequence_number: u32,
    pub bit_position: Option<u32>,
}

/// fx:PDU-TYPE according to https://www.asam.net/xml/fbx/fibex.xsd
/// It's a fx:REVISED-ELEMENT-TYPE extended by:
/// - byte-length: xs:unsignedInt
/// - pdu-type: fx:PDUTYPE (see the missing -)
/// - opt signal-instances
/// - opt multiplexer
/// - opt manufacturer-extension
#[derive(Debug)]
pub struct Pdu {
    // pub id: String, // duplicate? indirectly from pdus_map_by_id?
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub byte_length: u32,
    pub pdu_type: String, // todo better PDUTYPE?
    pub signal_instances: Vec<SignalInstance>,
    pub manufacturer_extension: Option<XmlElement>,
}

/// all elements are derived from fibex: REVISED-ELEMENT-TYPE containing:
/// - fx:NAMED-ELEMENT-TYPE with
///    - fx:IDENTIFIABLE-ELEMENT-TYPE optional attribute OID
///      - ID (typs xs:ID) reqd
///      - EXTERNAL-REFERENCES Option
/// - ELEMENT-REVISIONS: Option
/// - PRODUCT-REF: Option
#[derive(Debug)]
pub struct Elements {
    pub channels: HashMap<String, Channel>,
    pub ecus: Vec<Ecu>,
    pub pdus_map_by_id: HashMap<String, Pdu>,
    pub frames_map_by_id: HashMap<String, Frame>,
    pub signals_map_by_id: HashMap<String, Signal>,
    pub services_map_by_sid_major: HashMap<(u16, u8), Vec<Service>>,
    pub datatypes_map_by_id: HashMap<String, Datatype>,
}

/// service:GETTER-SETTER-TYPE and NOTIFIER-TYPE merged
#[derive(Debug)]
pub struct GetterSetterNotifier {
    pub method_identifier: u16, // todo use bigger type?
                                // return-code (for GETTER-SETTER only)
                                // reliable
                                // priority
                                // ...
                                // manufacturer-extension
}

/// todo add EVENTS.... (EVENT = service:METHOD-TYPE)

/// we do need to lookup most of the times by method id so optimize datastructure for it
#[derive(Debug)]
pub enum MethodIdType {
    Method(Method),
    Event(Method),
    Getter { field: Arc<Parameter> },
    Setter { field: Arc<Parameter> },
    Notifier { field: Arc<Parameter> },
}

/// service:SERVICE-INTERFACE-TYPE https://www.asam.net/xml/fbx/services/fibex4services.xsd
#[derive(Debug)]
pub struct Service {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub service_identifier: Option<u16>, // todo by spec its defined as a string... but as we target someip (or dlt) we use u16 (PRS_SOMEIP_00038)
    pub api_version: (u8, u8), // pair of major (the only one used on wire PRS_SOMEIP_00053), minor
    // methods: Vec<()>,
    // fields put directly into methods?
    pub fields: Vec<Arc<Parameter>>,
    // event-groups?
    pub methods_by_mid: HashMap<u16, MethodIdType>, // covers fields and events as well (getter, setter)
                                                    // MODIFIERS?
}

/// fx:SERIALIZATION-ATTRIBUTES-TYPE
///
#[derive(Debug, Clone)]
pub struct SerializationAttributes {
    /// optional length field size in bits
    pub length_field_size: Option<u32>,
    /// optional type field size in bits
    pub type_field_size: Option<u32>,
    pub array_length_field_size: Option<u32>,
    /// alignment of the datatype with dynamic bit-length. field will be zero padded to fulfill bit_alignment
    pub bit_alignment: Option<u32>,
    /// if set to true the serialization attributes are passed to the members of the complex datatype
    pub pass_on_to_subelements: Option<bool>,
}

/// fx:UTILIZATION-TYPE https://www.asam.net/xml/fbx/fibex.xsd
///
#[derive(Debug)]
pub struct Utilization {
    pub coding_ref: Option<String>,
    pub bit_length: Option<u32>,
    pub min_bit_length: Option<u32>,
    pub max_bit_length: Option<u32>,
    /// true = Big Endian
    pub is_high_low_byte_order: Option<bool>,
    pub serialization_attributes: Option<SerializationAttributes>,
}

/// service:PARAMETER-TYPE https://www.asam.net/xml/fbx/services/fibex4services.xsd
/// extended with opt. getter, setter, notifier to handle the Field as well
#[derive(Debug)]
pub struct Parameter {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub datatype_ref: String,
    pub position: i32,   // defaults to 0
    pub mandatory: bool, // defaults to true
    // need an example for that. pub data_id: Option<u32>,
    pub array_dimensions: Vec<ArrayDimension>,
    pub utilization: Option<Utilization>,

    // extended to cover FIELD-TYPE as well
    pub getter: Option<GetterSetterNotifier>,
    pub setter: Option<GetterSetterNotifier>,
    pub notifier: Option<GetterSetterNotifier>,
}

/// service:FIELD-TYPE https://www.asam.net/xml/fbx/services/fibex4services.xsd
/// mainly a Parameter with getter, setter, notifier. So we
/*
#[derive(Debug)]
pub struct Field {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub datatype_ref: String,
    pub array_dimensions: Vec<ArrayDimension>,
    pub utilization: Option<Utilization>,

    // ACCESS-PERMISSION
    // MODIFIERS todo?
    // the getter, setter, notifier are referenced from Service.methods_by_mid
    // we allow only one getter, setter, notifier. the spec only says minOccurs=0
    pub getter: Option<GetterSetterNotifier>,
    pub setter: Option<GetterSetterNotifier>,
    pub notifier: Option<GetterSetterNotifier>,
}*/

/// ARRAY-DIMENSION-TYPE: (can happen multiple time in seq and defines one dimension of the array each)
///  - BIT-ALIGNMENT uint32 opt. if e.g. 32 -> only for multi-dim arrays between end of the dimension to the next 32bit block padding
#[derive(Debug)]
pub struct ArrayDimension {
    pub dimension: u32,
    pub minimum_size: Option<u32>,
    pub maximum_size: Option<u32>, // >0
    pub bit_alignment: Option<u32>,
}

/* we added this into Parameter (adding data_id) and use Parameter for now
#[derive(Debug)]
pub struct ComplexDatatypeMember { // fx:COMPLEX-DATATYPE-MEMBER
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub datatype_ref: String,
    pub position: i32,
    //pub index: Option<u32>, // what is this used for???
    pub mandatory: Option<bool>,
    pub data_id: Option<u32>,
}*/

/// service:METHOD-TYPE https://www.asam.net/xml/fbx/services/fibex4services.xsd
/// todo: check params... vs. spec
#[derive(Debug)]
pub struct Method {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub method_identifier: Option<u16>, // non opt by spec but string todo
    pub input_params: Vec<Parameter>,
    pub return_params: Vec<Parameter>,
    // exceptions
    // return-code
    // reliable
    // modifiers
    // serialization-attributes
    // manufacturer-extensions
}

#[derive(Debug)]
pub struct Enum {
    pub value: i128, // max would be uint64 or sint64. todo refactor to use the type fitting to coding_ref
    pub synonym: Option<String>,
    pub desc: Option<String>,
}

#[derive(Debug)]
pub enum ComplexDatatypeClass {
    Structure,
    Union,
    Typedef,
}

#[derive(Debug)]
pub struct ComplexDatatype {
    pub class: ComplexDatatypeClass, // this has minOccurs=0 in fibex.xsd???
    pub members: Vec<Parameter>,
}

#[derive(Debug)]
pub enum DatatypeType {
    Common(String), // coding ref as only member
    ComplexType(ComplexDatatype),
    EnumType {
        coding_ref: String,
        enums: Vec<Enum>, // todo change into HashMap!
    },
}

#[derive(Debug)]
pub struct Datatype {
    pub id: String,
    pub short_name: Option<String>,
    pub desc: Option<String>,
    pub datatype: DatatypeType,
}

// todo change to reflect enums from ASAM_AE_MCD-2_NET_FIBEX 4.1.2

/// fibex ho:CATEGORY
#[derive(Debug)]
pub enum Category {
    LeadingLengthInfoType,
    EndOfPdu,
    MinMaxLengthType,
    StandardLengthType,
}

/// fibex ho:BASE-DATA-TYPE
///
#[derive(Debug)]
pub enum BaseDataType {
    AUint8,
    AInt8,
    AUint16,
    AInt16,
    AUint32,
    AInt32,
    AUint64,
    AInt64,
    AFloat32,
    AFloat64,
    AAsciiString,
    AUnicode2String,
    AByteField,
    ABitField,
    Other,
}

#[derive(Debug)]
pub enum Encoding {
    Signed,
    Unsigned,
    Bit,
    IeeeFloating,
    Bcd,
    BcdP,
    BcdUp,
    DspFractional,
    SM,
    E1C,
    E2C,
    Utf8,
    Utf16,
    Ucs2,
    Iso8859_1,
    Iso8859_2,
    Windows1252,
}

/// ho:TERMINATION type
#[derive(Debug)]
pub enum HoTermination {
    None,
    Zero,
    HexFF,
    Length,
}

/// fibex: ho:CODED-TYPE (see https://www.asam.net/xml/harmonizedObjects.xsd)
#[derive(Debug)]
pub struct CodedType {
    pub bit_length: Option<u32>, // xs:unsignedInt
    pub min_length: Option<u32>,
    pub max_length: Option<u32>,
    pub base_data_type: Option<BaseDataType>, // opt attr. ho:BASE-DATA-TYPE
    pub category: Category,                   // reqd attr. ho:CATEGORY
    pub encoding: Option<Encoding>,           // opt attr. ho:ENCODING
    pub termination: Option<HoTermination>,   // opt attr. ho:TERMINATION
}

/// fibex: CODING-TYPE
#[derive(Debug)]
pub struct Coding {
    pub id: String,
    pub short_name: Option<String>,
    // physical_type: Option
    pub coded_type: Option<CodedType>,
    pub compu_methods: Vec<CompuMethod>,
    // manufacturer-extension: Option
}

/// ho: COMPU-METHOD, see https://www.asam.net/xml/harmonizedObjects.xsd
/// - ho:NAME-DETAILS
/// - CATEGORY ho:COMPU-CATEGORY
/// - opt ho:UNIT-REF
/// - opt phys_contraints ho:SCALE-CONSTR-TYPE
/// - opt internal_constraints ho:SCALE-CONSTR-TYPE
/// - opt compu_internal_to_phys
///   - compu_scales array of ho:COMPU-SCALE
///   - opt compu_default_value
///
#[derive(Debug)]
pub struct CompuMethod {
    pub category: CompuCategory,
    pub internal_to_phys_scales: Vec<CompuScale>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompuCategory {
    Identical,
    Linear,
    ScaleLinear,
    TextTable,
    TabNointp,
    Formula,
    BitfieldTextTable, // new with ASAM MDF 4.2
}

/// enum representing more safely xs:double values
///
/// if possible we parse them into an i64 and if
/// the value is e.g. provided with a . we use the f64
#[derive(Debug, Clone)]
pub enum XsDouble {
    F64(f64),
    I64(i64),
}

impl From<&str> for XsDouble {
    fn from(value: &str) -> Self {
        if value.contains('.') {
            XsDouble::F64(value.parse::<f64>().unwrap_or(0f64))
        } else {
            XsDouble::I64(value.parse::<i64>().unwrap_or(0))
        }
    }
}
impl PartialEq for XsDouble {
    fn eq(&self, other: &Self) -> bool {
        match self {
            XsDouble::F64(a) => match other {
                XsDouble::F64(b) => a == b,
                XsDouble::I64(b) => *b as f64 == *a,
            },
            XsDouble::I64(a) => match other {
                XsDouble::I64(b) => a == b,
                XsDouble::F64(b) => *a as f64 == *b,
            },
        }
    }
}
impl Eq for XsDouble {}

impl PartialOrd for XsDouble {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            XsDouble::F64(a) => match other {
                XsDouble::F64(b) => a.partial_cmp(b),
                XsDouble::I64(b) => a.partial_cmp(&(*b as f64)),
            },
            XsDouble::I64(a) => match other {
                XsDouble::I64(b) => a.partial_cmp(b),
                XsDouble::F64(b) => (*a as f64).partial_cmp(b),
            },
        }
    }
}

/// ho:INTERVAL-TYPE
///  (xs:double, opt OPEN, CLOSED(default), INFINITE)
#[derive(Debug)]
pub struct IntervalType(pub std::ops::Bound<XsDouble>);

impl From<&XmlElement> for IntervalType {
    // parse e.g. <ho:LOWER-LIMIT INTERVAL-TYPE="CLOSED">1048576</ho:LOWER-LIMIT>
    fn from(xml_e: &XmlElement) -> Self {
        let v: XsDouble = xml_e.text.as_deref().unwrap_or("0").into();
        let v2 = v.to_owned(); // bit weird, could be avoided...
        let interval_type = xml_e
            .attr("INTERVAL-TYPE")
            .and_then(|a| match a.1.as_str() {
                "OPEN" => Some(std::ops::Bound::Excluded(v)),
                "INFINITE" => Some(std::ops::Bound::Unbounded),
                "CLOSED" => Some(std::ops::Bound::Included(v)),
                _ => None,
            })
            .unwrap_or(std::ops::Bound::Included(v2));
        Self(interval_type)
    }
}

impl IntervalType {
    pub fn partial_cmp(&self, other: &XsDouble) -> Option<std::cmp::Ordering> {
        match self {
            IntervalType(std::ops::Bound::Unbounded) => Some(std::cmp::Ordering::Equal),
            IntervalType(std::ops::Bound::Included(a)) => a.partial_cmp(other),
            IntervalType(std::ops::Bound::Excluded(a)) => {
                let c = a.partial_cmp(other);
                if let Some(std::cmp::Ordering::Equal) = c {
                    None
                } else {
                    c
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct CompuScale {
    // desc
    pub mask: Option<u64>, // or better type for mask? (64 bits enough?)
    pub lower_limit: Option<IntervalType>,
    pub upper_limit: Option<IntervalType>,
    pub compu_const: Option<VvT>, // todo not an option but an option of enum of COMPU-CONST, COMPU-RATIONAL-COEFFS, COMPU-GENERIC-MATH
}

#[derive(Debug, PartialEq, Eq)]
pub enum VvT {
    V(XsDouble),
    VT(String),
}

#[derive(Debug)]
pub struct ProcessingInformation {
    pub codings: HashMap<String, Coding>,
}

#[derive(Debug)]
pub struct FibexData {
    pub projects: Vec<Project>, // as we can hold info for multiple/super-set of projects
    pub elements: Elements,
    pub pi: ProcessingInformation,
}

impl FibexData {
    pub fn new() -> Self {
        FibexData {
            projects: vec![],
            elements: Elements {
                channels: HashMap::new(),
                ecus: Vec::new(),
                pdus_map_by_id: HashMap::new(),
                frames_map_by_id: HashMap::new(),
                datatypes_map_by_id: HashMap::new(),
                services_map_by_sid_major: HashMap::new(),
                signals_map_by_id: HashMap::new(),
            },
            pi: ProcessingInformation {
                codings: HashMap::new(),
            },
        }
    }
    pub fn load_fibex_file(&mut self, file: &Path) -> Result<(), Box<dyn Error>> {
        let mut reader = Reader::from_file(file)?;
        let mut buf = Vec::new();
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) if e.local_name() == b"FIBEX" => {
                    self.parse_fibex(e, &mut reader)?
                }
                Event::Start(ref e) | Event::Empty(ref e) => {
                    println!(
                        "load_fibex_file: unexpected Event of '{}' treating as no fibex!",
                        String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                    );
                    return Err(FibexError::new("expecting only FIBEX tag").into());
                }
                Event::Eof => break,
                _ => {}
            }
        }
        Ok(())
    }

    fn parse_fibex<T: BufRead>(
        &mut self,
        fibex: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::new();
        loop {
            // todo match only for project, elements, processing-information
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"PROJECT" => {
                        let proj = self.parse_project(e, reader)?;
                        self.projects.push(proj);
                    }
                    b"ELEMENTS" => self.parse_elements(e, reader)?,
                    b"PROCESSING-INFORMATION" => self.parse_pi(e, reader)?,
                    _ => {
                        println!(
                            "parse_fibex: unprocessed Event::Start of '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?;
                    }
                },
                Event::Empty(ref e) => println!(
                    "parse_fibex: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == fibex.local_name() => break,
                _ => {}
            }
        }
        Ok(())
    }

    fn parse_project<T: BufRead>(
        &mut self,
        e_project: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<Project, Box<dyn Error>> {
        let xml_e = read_element(e_project, reader, false)?;
        let id = xml_e
            .attr("ID")
            .ok_or_else(|| FibexError::new("ID missing for project"))?;
        let proj = Project {
            id: id.1.to_owned(),
            short_name: xml_e
                .child_by_name("SHORT-NAME")
                .and_then(|c| c.text.to_owned()),
        };

        Ok(proj)
    }

    fn parse_elements<T: BufRead>(
        &mut self,
        e_elements: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::new();
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    // todo CHANNELS, ECUS, PACKAGES
                    b"SERVICE-INTERFACES" => self.parse_service_interfaces(e, reader)?,
                    b"DATATYPES" => self.parse_datatypes(e, reader)?,
                    b"CHANNELS" | b"ECUS" | b"FRAMES" | b"PDUS" | b"SIGNALS" => {} // ignore, fall through
                    b"ECU" => self.parse_ecu(e, reader)?,
                    b"FRAME" => self.parse_frame(e, reader)?,
                    b"SIGNAL" => self.parse_signal(e, reader)?,
                    b"PDU" => self.parse_pdu(e, reader)?,
                    b"CHANNEL" => self.parse_channel(e, reader)?,
                    _ => {
                        println!(
                            "parse_elements: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        // skip element to keep the recursive order.
                        skip_element(e, reader)?;
                    }
                },
                Event::Empty(ref e) => println!(
                    "parse_elements: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_elements.local_name() => break,
                _ => {}
            }
        }
        Ok(())
    }

    fn parse_service_interfaces<T: BufRead>(
        &mut self,
        e_si: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::new();
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SERVICE-INTERFACE" => {
                        let si = self.parse_service_interface(e, reader)?; // todo skip single failures?
                        let key = (si.service_identifier.unwrap_or_default(), si.api_version.0);
                        self.elements
                            .services_map_by_sid_major
                            .entry(key)
                            .or_default()
                            .push(si);
                    }
                    _ => {
                        println!(
                            "parse_service_interfaces: unprocessed Event::Start of '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?;
                    }
                },
                Event::Empty(ref e) => println!(
                    "parse_service_interface: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_si.local_name() => break,
                _ => {}
            }
        }
        Ok(())
    }
    fn parse_service_interface<T: BufRead>(
        &mut self,
        e_si: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<Service, Box<dyn Error>> {
        let mut buf = Vec::with_capacity(64 * 1024); // todo better default
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut api_version: (u8, u8) = (0, 0);
        let mut service_identifier: Option<u16> = None;
        let mut methods_by_mid = HashMap::new();
        let mut fields = vec![];

        let id = e_si
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in Service"))?;

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"SERVICE-IDENTIFIER" => {
                        service_identifier = Some(
                            reader
                                .read_text(e.name(), &mut Vec::new())?
                                .parse::<u16>()?,
                        )
                    }
                    b"API-VERSION" => {} // we ignore this and wait for the MAJOR/MINOR
                    b"MAJOR" => {
                        api_version.0 =
                            reader.read_text(e.name(), &mut Vec::new())?.parse::<u8>()?
                    }
                    b"MINOR" => {
                        api_version.1 =
                            reader.read_text(e.name(), &mut Vec::new())?.parse::<u8>()?
                    }
                    b"METHODS" => {} // we ignore to get the method events
                    b"METHOD" | b"EVENT" => {
                        let (mid, key) = self.parse_method(e, reader)?;
                        methods_by_mid.insert(key, mid);
                        // todo ignore duplicates?
                    }
                    b"FIELDS" | b"EVENTS" => {} // we ignore to get the FIELD/EVENT events
                    b"FIELD" => {
                        let field = Arc::new(self.parse_parameter(e, reader, true)?);

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

                        fields.push(field);
                    }
                    b"EVENT-GROUPS" => skip_element(e, reader)?, // todo!
                    _ => {
                        println!(
                            "parse_service_interface: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) => println!(
                    "parse_service_interface: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_si.local_name() => break,
                _ => {}
            }
        }
        let si = Service {
            id,
            short_name,
            desc,
            api_version,
            service_identifier,
            fields,
            methods_by_mid,
        };
        Ok(si)
    }

    fn parse_channel<T: BufRead>(
        &mut self,
        e_si: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::with_capacity(1024 * 1024); // todo better default they are large
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut frame_ref_by_frame_triggering_identifier = HashMap::new();
        let mut manufacturer_extension: Option<XmlElement> = None;

        let id = e_si
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in CHANNEL"))?;

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"MANUFACTURER-EXTENSION" => {
                        manufacturer_extension = Some(read_element(e, reader, false)?)
                    }
                    b"PDU-TRIGGERINGS" => skip_element(e, reader)?, // todo
                    b"LONG-NAME" | b"ELEMENT-REVISIONS" | b"FLEXRAY-CHANNEL-NAME" => {
                        skip_element(e, reader)?
                    } // todo add once needed
                    b"FRAME-TRIGGERINGS" => {}                      // fallthrough
                    b"FRAME-TRIGGERING" => {
                        // for now we dont read the full element but just the IDENTIFIER / FRAME-REF mapping
                        let ft = read_element(e, reader, false)?;
                        let identifier = ft
                            .child_by_name("IDENTIFIER")
                            .and_then(|identifier| identifier.child_by_name("IDENTIFIER-VALUE"))
                            .and_then(|value| value.text.as_ref())
                            .and_then(|text| text.parse::<u32>().ok());
                        if let Some(identifier) = identifier {
                            if identifier > 0 {
                                let frame_ref = ft
                                    .child_by_name("FRAME-REF")
                                    .and_then(|e| e.attr("ID-REF"))
                                    .map(|a| a.1.to_owned());
                                if let Some(frame_ref) = frame_ref {
                                    // insert mapping
                                    frame_ref_by_frame_triggering_identifier
                                        .insert(identifier, frame_ref);
                                } else {
                                    println!(
                                    "parse_channel: ignoring FRAME-TRIGGERING ID'{:?}' due to missing FRAME-REF",
                                    ft.attr("ID")
                                )
                                }
                            } else {
                                println!(
                                    "parse_channel: ignoring FRAME-TRIGGERING ID'{:?}' due to IDENTIFIER=0",
                                    ft.attr("ID")
                                )
                            }
                        } /* quite common case for e.g. flexray else{
                              println!(
                                  "parse_channel: ignoring FRAME-TRIGGERING ID'{:?}' due to missing IDENTIFIER",
                                  ft.attr("ID")
                              )
                          }*/
                    }
                    _ => {
                        println!(
                            "parse_channel: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) => println!(
                    "parse_channel: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_si.local_name() => break,
                _ => {}
            }
        }
        let channel = Channel {
            id,
            short_name,
            desc,
            frame_ref_by_frame_triggering_identifier,
            manufacturer_extension,
        };
        // does a channel with same id exist yet?
        if let Some(exist_channel) = self.elements.channels.get_mut(&channel.id) {
            // update/merge existing one:
            match &exist_channel.short_name {
                None => exist_channel.short_name = channel.short_name,
                Some(exist_name) => {
                    if let Some(new_name) = &channel.short_name {
                        if new_name != exist_name {
                            println!(
                                "parse_channel: merge channel id '{}' different names exist: {} vs. new (ignored) {}", channel.id, exist_name, new_name
                            );
                        }
                    }
                }
            }
            if exist_channel.desc.is_none() {
                exist_channel.desc = channel.desc;
            }
            if exist_channel.manufacturer_extension.is_none() {
                exist_channel.manufacturer_extension = channel.manufacturer_extension;
            }
            // insert missing frames:
            for new_chan in channel.frame_ref_by_frame_triggering_identifier.into_iter() {
                exist_channel.frame_ref_by_frame_triggering_identifier.entry(new_chan.0)
                .and_modify(|ref_id|{if ref_id != &new_chan.1 {
                         println!("parse_channel: merge channel id '{}' frame {} different ref exist {} vs. {}", channel.id, new_chan.0, ref_id, new_chan.1);
                    }}).or_insert(new_chan.1);
            }
        } else {
        self.elements
            .channels
            .insert(channel.id.to_owned(), channel);
        }
        Ok(())
    }

    fn parse_ecu<T: BufRead>(
        &mut self,
        e_si: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::with_capacity(64 * 1024); // todo better default
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut manufacturer_extension: Option<XmlElement> = None;

        let id = e_si
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in ECU"))?;

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"MANUFACTURER-EXTENSION" => {
                        manufacturer_extension = Some(read_element(e, reader, false)?)
                    }
                    b"CONNECTORS" => skip_element(e, reader)?, // todo
                    b"LONG-NAME"
                    | b"ELEMENT-REVISIONS"
                    | b"DIAGNOSTIC-ADDRESSES"
                    | b"CONTROLLERS" => skip_element(e, reader)?, // todo add once needed
                    _ => {
                        println!(
                            "parse_ecu: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) => println!(
                    "parse_ecu: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_si.local_name() => break,
                _ => {}
            }
        }
        let ecu = Ecu {
            id,
            short_name,
            desc,
            manufacturer_extension,
        };
        self.elements.ecus.push(ecu);
        Ok(())
    }

    fn parse_frame<T: BufRead>(
        &mut self,
        e_si: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::with_capacity(16 * 1024); // todo better default
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut manufacturer_extension: Option<XmlElement> = None;
        let mut byte_length: Option<u32> = None;
        let mut frame_type: Option<String> = None;
        let mut pdu_instances = vec![];

        let id = e_si
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in FRAME"))?;

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"FRAME-TYPE" => {
                        frame_type = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"BYTE-LENGTH" => {
                        byte_length = Some(
                            reader
                                .read_text(e.name(), &mut Vec::new())?
                                .parse::<u32>()?,
                        )
                    }
                    b"MANUFACTURER-EXTENSION" => {
                        manufacturer_extension = Some(read_element(e, reader, false)?)
                    }
                    b"PDU-INSTANCES" => {} // ignore and process PDU-INSTANCE directly here
                    b"PDU-INSTANCE" => {
                        let pdu_i = read_element(e, reader, false)?;

                        let pdu_ref = pdu_i
                            .child_by_name("PDU-REF")
                            .and_then(|e| e.attr("ID-REF"))
                            .map(|a| a.1.to_owned())
                            .ok_or_else(|| {
                                FibexError::new("pdu_ref missing in FRAME/PDU-INSTACNE")
                            })?;
                        let sequence_nr = pdu_i
                            .child_by_name("SEQUENCE-NUMBER")
                            .and_then(|e| e.text.as_ref())
                            .and_then(|e| e.parse::<u32>().ok())
                            .or(Some(pdu_instances.len() as u32)) // assert BIT-POSITION?
                            .ok_or_else(|| FibexError {
                                msg: format!(
                                    "SEQUENCE-NR missing in FRAME/PDU-INSTANCE short_name={:?}",
                                    short_name
                                ),
                            })?;

                        let bit_position = pdu_i
                            .child_by_name("BIT-POSITION")
                            .and_then(|e| e.text.as_ref())
                            .and_then(|e| e.parse::<u32>().ok());
                        let is_high_low_byte_order = pdu_i
                            .child_by_name("IS-HIGH-LOW-BYTE-ORDER")
                            .and_then(|e| e.text.as_deref())
                            .and_then(|t| t.parse::<bool>().ok()); // todo handle errors? (instead of ok!)
                        if pdu_instances.len() as u32 != sequence_nr {
                            return Err(FibexError {
                                msg: format!(
                                    "SEQUENCE-NR {} vs {} mismatch in FRAME/PDU-INSTANCE",
                                    sequence_nr,
                                    pdu_instances.len()
                                ),
                            }
                            .into());
                        }
                        pdu_instances.push(PduInstance {
                            pdu_ref,
                            bit_position,
                            is_high_low_byte_order,
                        });
                    }
                    b"LONG-NAME" => skip_element(e, reader)?, // todo
                    b"ELEMENT-REVISIONS" => skip_element(e, reader)?,
                    _ => {
                        println!(
                            "parse_frame: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) => println!(
                    "parse_frame: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_si.local_name() => break,
                _ => {}
            }
        }

        let byte_length = if let Some(byte_length) = byte_length {
            byte_length
        } else {
            return Err(FibexError::new("BYTE-LENGTH missing in FRAME").into());
        };

        let frame = Frame {
            id,
            short_name,
            desc,
            byte_length,
            frame_type,
            pdu_instances,
            manufacturer_extension,
        };
        self.elements
            .frames_map_by_id
            .insert(frame.id.clone(), frame);
        Ok(())
    }

    fn parse_signal<T: BufRead>(
        &mut self,
        e_si: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::with_capacity(512); // todo better default
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut manufacturer_extension: Option<XmlElement> = None;
        let mut coding_ref = None;

        let id = e_si
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in SIGNAL"))?;

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"MANUFACTURER-EXTENSION" => {
                        manufacturer_extension = Some(read_element(e, reader, false)?)
                    }
                    b"LONG-NAME" => skip_element(e, reader)?, // todo
                    b"ELEMENT-REVISIONS" | b"SIGNAL-TYPE" => skip_element(e, reader)?,
                    _ => {
                        println!(
                            "parse_signal: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"CODING-REF" => {
                    let r = read_element(e, reader, true)?;
                    if let Some((_k, v)) = r.attr("ID-REF") {
                        // ID-REF is mandatory according to fibex.xsd todo could throw error if not
                        coding_ref = Some(v.to_owned());
                    }
                }
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) => println!(
                    "parse_signal: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_si.local_name() => break,
                _ => {}
            }
        }

        let coding_ref = if let Some(coding_ref) = coding_ref {
            coding_ref
        } else {
            return Err(FibexError::new("CODING-REF missing in SIGNAL").into());
        };

        let signal = Signal {
            id,
            short_name,
            desc,
            coding_ref,
            manufacturer_extension,
        };
        self.elements
            .signals_map_by_id
            .insert(signal.id.clone(), signal);
        Ok(())
    }

    fn parse_pdu<T: BufRead>(
        &mut self,
        e_si: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::with_capacity(16 * 1024); // todo better default
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut manufacturer_extension: Option<XmlElement> = None;
        let mut byte_length: Option<u32> = None;
        let mut pdu_type: Option<String> = None;
        let mut signal_instances = vec![];

        let id = e_si
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in PDU"))?;

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"PDU-TYPE" => pdu_type = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"BYTE-LENGTH" => {
                        byte_length = Some(
                            reader
                                .read_text(e.name(), &mut Vec::new())?
                                .parse::<u32>()?,
                        )
                    }
                    b"MANUFACTURER-EXTENSION" => {
                        manufacturer_extension = Some(read_element(e, reader, false)?)
                    }
                    b"SIGNAL-INSTANCES" => {} // ignore and process PDU-INSTANCE directly here
                    b"SIGNAL-INSTANCE" => {
                        let pdu_i = read_element(e, reader, false)?;

                        let signal_ref = pdu_i
                            .child_by_name("SIGNAL-REF")
                            .and_then(|e| e.attr("ID-REF"))
                            .map(|a| a.1.to_owned())
                            .ok_or_else(|| {
                                FibexError::new("SIGNAL-REF missing in FRAME/SIGNAL-INSTACNE")
                            })?;
                        let sequence_nr = pdu_i
                            .child_by_name("SEQUENCE-NUMBER")
                            .and_then(|e| e.text.as_ref())
                            .and_then(|e| e.parse::<u32>().ok())
                            .or(Some(signal_instances.len() as u32)) // assert a BIT-POSITION then?
                            .ok_or_else(|| FibexError {
                                msg: format!(
                                    "SEQUENCE-NR missing in FRAME/SIGNAL-INSTANCE, short_name={:?}",
                                    short_name
                                ),
                            })?;
                        let bit_position = pdu_i
                            .child_by_name("BIT-POSITION")
                            .and_then(|e| e.text.as_ref())
                            .and_then(|e| e.parse::<u32>().ok());
                        let is_high_low_byte_order = pdu_i
                            .child_by_name("IS-HIGH-LOW-BYTE-ORDER")
                            .and_then(|e| e.text.as_deref())
                            .and_then(|t| t.parse::<bool>().ok()); 
                        if signal_instances.len() as u32 != sequence_nr {
                            return Err(FibexError {
                                msg: format!(
                                    "SEQUENCE-NR {} vs {} mismatch in FRAME/SIGNAL-INSTANCE",
                                    sequence_nr,
                                    signal_instances.len()
                                ),
                            }
                            .into());
                        }
                        signal_instances.push(SignalInstance {
                            signal_ref,
                            is_high_low_byte_order,
                            bit_position,
                        });
                    }
                    b"LONG-NAME" => skip_element(e, reader)?, // todo
                    b"ELEMENT-REVISIONS" => skip_element(e, reader)?, // todo
                    _ => {
                        println!(
                            "parse_pdu: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) => println!(
                    "parse_pdu: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_si.local_name() => break,
                _ => {}
            }
        }

        let byte_length = if let Some(byte_length) = byte_length {
            byte_length
        } else {
            return Err(FibexError::new("BYTE-LENGTH missing in PDU").into());
        };
        let pdu_type = if let Some(pdu_type) = pdu_type {
            pdu_type
        } else {
            return Err(FibexError::new("PDU-TYPE missing in PDU").into());
        };

        let pdu = Pdu {
            //id,
            short_name,
            desc,
            byte_length,
            pdu_type,
            signal_instances,
            manufacturer_extension,
        };
        self.elements.pdus_map_by_id.insert(id, pdu);
        Ok(())
    }

    fn parse_method<T: BufRead>(
        &mut self,
        e_method: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(MethodIdType, u16), Box<dyn Error>> {
        let mut buf = Vec::with_capacity(64 * 1024); // todo better default
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut method_identifier: Option<u16> = None;
        let mut input_params = vec![];
        let mut return_params = vec![];

        let id = e_method
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in Method"))?;

        let is_event = e_method.local_name() == b"EVENT";

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"METHOD-IDENTIFIER" => {
                        method_identifier = Some(
                            reader
                                .read_text(e.name(), &mut Vec::new())?
                                .parse::<u16>()?,
                        )
                    }
                    b"INPUT-PARAMETERS" | b"RETURN-PARAMETERS" => {} // ignore, we parse the parameters directly
                    b"INPUT-PARAMETER" | b"RETURN-PARAMETER" => {
                        let param = self.parse_parameter(e, reader, false);
                        if let Ok(param) = param {
                            match e.local_name() {
                                b"INPUT-PARAMETER" => &mut input_params,
                                _ => &mut return_params,
                            }
                            .push(param);
                        } else {
                            println!(
                                "parse_method: Ignoring parameter due to Err '{}'",
                                param.unwrap_err()
                            )
                        }
                        //let key = method.method_identifier.unwrap_or_default();
                        //methods_by_mid.insert(key, method); // todo ignore duplicates?
                    }
                    b"RELIABLE" | b"MANUFACTURER-EXTENSION" | b"CALL-SEMANTIC" => {
                        skip_element(e, reader)?
                    } // todo!
                    _ => {
                        println!(
                            "parse_method: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) => println!(
                    "parse_method: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_method.local_name() => break,
                _ => {}
            }
        }

        // sort params by position:
        input_params.sort_by(|a, b| a.position.cmp(&b.position));
        return_params.sort_by(|a, b| a.position.cmp(&b.position));

        let key = &method_identifier.unwrap_or_default();

        let m = Method {
            id,
            method_identifier,
            short_name,
            desc,
            input_params,
            return_params,
        };
        let mid = if is_event {
            MethodIdType::Event(m)
        } else {
            MethodIdType::Method(m)
        };
        Ok((mid, *key))
    }

    fn parse_parameter<T: BufRead>(
        &mut self,
        e_pa: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
        is_field: bool,
    ) -> Result<Parameter, Box<dyn Error>> {
        let mut buf = Vec::with_capacity(4 * 1024); // todo better default
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut datatype_ref: Option<String> = None;
        let mut mandatory: bool = true;

        let mut position: Option<i32> = None;
        let mut array_dimensions = vec![];
        let mut utilization: Option<Utilization> = None;
        let mut getter = None;
        let mut setter = None;
        let mut notifier = None;

        let id = e_pa
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in Parameter"))?;

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    b"DATATYPE-REF" => {
                        datatype_ref = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"POSITION" => {
                        position = Some(
                            reader
                                .read_text(e.name(), &mut Vec::new())?
                                .parse::<i32>()?,
                        )
                    }
                    b"MANDATORY" => {
                        mandatory = reader
                            .read_text(e.name(), &mut Vec::new())?
                            .parse::<bool>()?;
                    }
                    b"UTILIZATION" => {
                        let ut = read_element(e, reader, false)?;
                        utilization = Some(Utilization {
                            coding_ref: ut
                                .child_by_name("CODING-REF")
                                .and_then(|e| e.attr("ID-REF"))
                                .map(|a| a.1.to_owned()),
                            bit_length: ut
                                .child_by_name("BIT-LENGTH")
                                .and_then(|e| e.text.as_deref())
                                .and_then(|t| t.parse::<u32>().ok()),
                            min_bit_length: ut
                                .child_by_name("MIN-BIT-LENGTH")
                                .and_then(|e| e.text.as_deref())
                                .and_then(|t| t.parse::<u32>().ok()),
                            max_bit_length: ut
                                .child_by_name("MAX-BIT-LENGTH")
                                .and_then(|e| e.text.as_deref())
                                .and_then(|t| t.parse::<u32>().ok()),
                            is_high_low_byte_order: ut
                                .child_by_name("IS-HIGH-LOW-BYTE-ORDER")
                                .and_then(|e| e.text.as_deref())
                                .and_then(|t| t.parse::<bool>().ok()), // todo handle errors? (instead of ok!)
                            serialization_attributes: {
                                ut.child_by_name("SERIALIZATION-ATTRIBUTES").map(|sa| {
                                    SerializationAttributes {
                                        length_field_size: sa
                                            .child_by_name("LENGTH-FIELD-SIZE")
                                            .and_then(|e| e.text.as_deref())
                                            .and_then(|t| t.parse::<u32>().ok()),
                                        type_field_size: sa
                                            .child_by_name("TYPE-FIELD-SIZE")
                                            .and_then(|e| e.text.as_deref())
                                            .and_then(|t| t.parse::<u32>().ok()),
                                        bit_alignment: sa
                                            .child_by_name("BIT-ALIGNMENT")
                                            .and_then(|e| e.text.as_deref())
                                            .and_then(|t| t.parse::<u32>().ok()),
                                        array_length_field_size: sa
                                            .child_by_name("ARRAY-LENGTH-FIELD-SIZE")
                                            .and_then(|e| e.text.as_deref())
                                            .and_then(|t| t.parse::<u32>().ok()),
                                        pass_on_to_subelements: sa
                                            .child_by_name("PASS-ON-TO-SUBELEMENTS")
                                            .and_then(|e| e.text.as_deref())
                                            .and_then(|t| t.parse::<bool>().ok()),
                                    }
                                })
                            },
                        });
                    }
                    b"GETTER" | b"SETTER" | b"NOTIFIER" if is_field => {
                        // to support FIELDs
                        // todo change into own method
                        let xe = read_element(e, reader, false)?;
                        let mid = xe
                            .child_by_name("METHOD-IDENTIFIER")
                            .or_else(|| xe.child_by_name("NOTIFICATION-IDENTIFIER"))
                            .and_then(|e| {
                                if let Some(text) = &e.text {
                                    text.parse::<u16>().ok()
                                } else {
                                    None
                                }
                            })
                            .ok_or_else(|| FibexError {
                                msg: format!("METHOD-IDENTIFIER missing in FIELD {}", id),
                            })?;
                        let gsn = GetterSetterNotifier {
                            method_identifier: mid,
                        };
                        match e.local_name() {
                            b"GETTER" => getter = Some(gsn),
                            b"SETTER" => setter = Some(gsn), // todo overwrite prev one? err?
                            _ => notifier = Some(gsn),
                        }
                    }

                    b"ARRAY-DECLARATION" => {} // ignore, use array-dimension directly
                    b"ARRAY-DIMENSION" => {
                        let ad = read_element(e, reader, false)?;
                        if let Some(xd) = ad.child_by_name("DIMENSION") {
                            array_dimensions.push(ArrayDimension {
                                dimension: xd
                                    .text
                                    .as_ref()
                                    .ok_or_else(|| {
                                        Box::new(FibexError {
                                            msg: format!(
                                                "DATATYPE-REF missing for PARAMETER ID={}",
                                                id
                                            ),
                                        })
                                    })?
                                    .parse::<u32>()?,
                                minimum_size: ad
                                    .child_by_name("MINIMUM-SIZE")
                                    .and_then(|e| e.text.as_deref())
                                    .and_then(|t| t.parse::<u32>().ok()),
                                maximum_size: ad
                                    .child_by_name("MAXIMUM-SIZE")
                                    .and_then(|e| e.text.as_deref())
                                    .and_then(|t| t.parse::<u32>().ok()),
                                bit_alignment: ad
                                    .child_by_name("BIT-ALIGNMENT")
                                    .and_then(|e| e.text.as_deref())
                                    .and_then(|t| t.parse::<u32>().ok()),
                            });
                        } else {
                            return Err(Box::new(FibexError {
                                msg: format!("DIMENSION missing for ARRAY-DIMENSION ID={}", id),
                            }));
                        }
                    }
                    b"ACCESS-PERMISSION" => skip_element(e, reader)?,
                    _ => {
                        println!(
                            "parse_parameter: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) if e.local_name() == b"DATATYPE-REF" => {
                    let r = read_element(e, reader, true)?;
                    if let Some((_k, v)) = r.attr("ID-REF") {
                        datatype_ref = Some(v.to_owned());
                    }
                }
                Event::Empty(ref e) => println!(
                    "parse_parameter: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_pa.local_name() => break,
                _ => {}
            }
        }

        let datatype_ref = datatype_ref.ok_or_else(|| {
            Box::new(FibexError {
                msg: format!("DATATYPE-REF missing for PARAMETER ID={}", id),
            })
        })?;

        let position = position.unwrap_or(0);

        let p = Parameter {
            id,
            position,
            short_name,
            desc,
            datatype_ref,
            utilization,
            mandatory,
            array_dimensions,
            getter,
            setter,
            notifier,
        };
        Ok(p)
    }
    fn parse_datatypes<T: BufRead>(
        &mut self,
        e_dt: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::new();
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"DATATYPE" => {
                        let dt = self.parse_datatype(e, reader)?; // todo skip single failures?
                        self.elements
                            .datatypes_map_by_id
                            .insert(dt.id.to_owned(), dt); // todo check for dupl? and avoid id.clone
                    }
                    _ => {
                        println!(
                            "parse_datatypes: unprocessed Event::Start of '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?;
                    }
                },
                Event::Empty(ref e) => println!(
                    "parse_datatypes: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_dt.local_name() => break,
                _ => {}
            }
        }
        Ok(())
    }

    fn parse_datatype<T: BufRead>(
        &mut self,
        e_dt: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<Datatype, Box<dyn Error>> {
        let mut buf = Vec::with_capacity(64 * 1024); // todo better default
        let mut short_name: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut enums = vec![];
        let mut datatype_class = None;
        let mut members: Vec<Parameter> = vec![];

        let id = e_dt
            .attributes()
            .flatten()
            .find(|a| a.key == b"ID")
            .and_then(|attribute| String::from_utf8(attribute.value.to_vec()).ok())
            .ok_or_else(|| FibexError::new("ID missing in DATATYPE"))?;

        let etype = e_dt
            .attributes()
            .flatten()
            .find(|a| a.key == b"type" || a.key.ends_with(b":type"))
            .map(|attribute| attribute.value.to_vec());

        let mut coding_ref: Option<String> = None;

        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"SHORT-NAME" => {
                        short_name = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"DESC" => desc = Some(reader.read_text(e.name(), &mut Vec::new())?),
                    //                    b"FIELDS" | b"EVENTS" | b"EVENT-GROUPS" => skip_element(e, reader)?, // todo!
                    b"ENUMERATION-ELEMENTS" => {} // skip, we parse the ENUM-ELEMENT here
                    b"ENUM-ELEMENT" => {
                        let r = read_element(e, reader, false)?;
                        let v = r.child_by_name("VALUE").and_then(|c| c.text.to_owned());
                        if let Some(v) = v {
                            let s = r.child_by_name("SYNONYM").and_then(|c| c.text.to_owned());
                            enums.push(Enum {
                                value: v.parse::<i128>().unwrap_or(0),
                                synonym: s,
                                desc: None,
                            });
                        }
                    }
                    b"COMPLEX-DATATYPE-CLASS" => {
                        datatype_class = Some(reader.read_text(e.name(), &mut Vec::new())?)
                    }
                    b"MEMBERS" => {} // ignore we parse MEMBER directly
                    b"MEMBER" => match self.parse_parameter(e, reader, false) {
                        Ok(param) => members.push(param),
                        Err(e) => {
                            // e.g. if DATATYPE-REF is missing/empty (some faulty fibex generators)
                            println!("parse_datatype: MEMBERS skipping MEMBER due to Err '{}'", e);
                        }
                    },
                    _ => {
                        println!(
                            "parse_datatype: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) if e.local_name() == b"PACKAGE-REF" => {} // todo ignore for now
                Event::Empty(ref e) if e.local_name() == b"CODING-REF" => {
                    let r = read_element(e, reader, true)?;
                    if let Some((_k, v)) = r.attr("ID-REF") {
                        // ID-REF is mandatory according to fibex.xsd todo could throw error if not
                        coding_ref = Some(v.to_owned());
                    }
                }
                Event::Empty(ref e) => println!(
                    "parse_datatype: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == e_dt.local_name() => break,
                _ => {}
            }
        }

        let datatype = match etype {
            Some(s) if s == b"fx:ENUM-DATATYPE-TYPE" => DatatypeType::EnumType {
                coding_ref: coding_ref.ok_or_else(|| {
                    Box::new(FibexError::new("CODING-REF missing for ENUM-DATATYPE-TYPE"))
                })?,
                enums,
            },
            Some(s) if s == b"fx:COMPLEX-DATATYPE-TYPE" => {
                let class = match datatype_class.as_deref() {
                    Some("TYPEDEF") => ComplexDatatypeClass::Typedef,
                    Some("STRUCTURE") => ComplexDatatypeClass::Structure,
                    Some("UNION") => ComplexDatatypeClass::Union,
                    _ => {
                        return Err(Box::new(FibexError{msg:
                            format!("COMPLEX-DATATYPE-CLASS missing/unknown ({:?}) for COMPLEX-DATATYPE-TYPE", datatype_class,
                        )}));
                    }
                };
                members.sort_by(|a, b| a.position.cmp(&b.position));
                DatatypeType::ComplexType(ComplexDatatype { class, members })
            }
            Some(s) if s == b"fx:COMMON-DATATYPE-TYPE" => {
                DatatypeType::Common(coding_ref.ok_or_else(|| {
                    Box::new(FibexError::new(
                        "CODING-REF missing for COMMON-DATATYPE-TYPE",
                    ))
                })?)
            }
            Some(s) => {
                println!(
                    "parse_datatype: unhandled etype '{}'",
                    String::from_utf8(s).unwrap_or_default()
                );
                return Err(FibexError::new("unknown type for DATATYPE").into());
            }
            _ => {
                println!("parse_datatype: unhandled etype '{:?}'", etype);
                return Err(FibexError::new("unknown/missing type for DATATYPE").into());
            }
        };

        let dt = Datatype {
            id,
            short_name,
            desc,
            datatype,
        };
        Ok(dt)
    }

    fn parse_pi<T: BufRead>(
        &mut self,
        pi: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::new();
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"CODINGS" => self.parse_codings(e, reader)?,
                    _ => {
                        println!(
                            "parse_pi: unprocessed Event::Start of '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) => println!(
                    "parse_pi: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == pi.local_name() => break,
                _ => {}
            }
        }
        Ok(())
    }

    fn parse_codings<T: BufRead>(
        &mut self,
        codings: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<(), Box<dyn Error>> {
        let mut buf = Vec::new();
        loop {
            match reader.read_event(&mut buf)? {
                Event::Start(ref e) => match e.local_name() {
                    b"CODING" => {
                        let coding = self.parse_coding(e, reader)?;
                        self.pi.codings.insert(coding.id.clone(), coding);
                    }
                    _ => {
                        println!(
                            "parse_codings: Event::Start of unknown '{}'",
                            String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                        );
                        skip_element(e, reader)?
                    }
                },
                Event::Empty(ref e) => println!(
                    "parse_codings: Event::Empty of unknown '{}'",
                    String::from_utf8(e.local_name().to_vec()).unwrap_or_default()
                ),
                Event::End(ref e) if e.local_name() == codings.local_name() => break,
                _ => {}
            }
        }
        Ok(())
    }

    fn parse_coding<T: BufRead>(
        &mut self,
        e: &quick_xml::events::BytesStart,
        reader: &mut Reader<T>,
    ) -> Result<Coding, Box<dyn Error>> {
        let xml_e = read_element(e, reader, false)?;
        let id = xml_e
            .attr("ID")
            .ok_or_else(|| FibexError::new("ID missing for coding"))?;
        let cod = Coding {
            id: id.1.to_owned(),
            short_name: xml_e
                .child_by_name("SHORT-NAME")
                .and_then(|c| c.text.to_owned()),
            coded_type: xml_e
                .child_by_name("CODED-TYPE")
                .and_then(|c| CodedType::from_xml(c).ok()), // todo dont discard the error!
            compu_methods: xml_e
                .child_by_name("COMPU-METHODS")
                .map(|ms| {
                    ms.children
                        .iter()
                        .filter_map(|m| CompuMethod::from_xml(m).ok())
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default(),
        };

        Ok(cod)
    }
}

impl CompuMethod {
    fn from_xml(xml_e: &XmlElement) -> Result<CompuMethod, Box<dyn Error>> {
        let category = xml_e
            .child_by_name("CATEGORY")
            .and_then(|c| c.text.as_deref());
        let category = match category {
            Some(cat) if cat == "IDENTICAL" => CompuCategory::Identical,
            Some(cat) if cat == "BITFIELD-TEXTTABLE" => CompuCategory::BitfieldTextTable,
            Some(cat) if cat == "LINEAR" => CompuCategory::Linear,
            Some(cat) if cat == "TEXTTABLE" => CompuCategory::TextTable,
            Some(cat) => {
                println!("CompuMethod::from_xml: unknown CATEGORY {}", cat); // todo...
                return Err(FibexError {
                    msg: format!("CompuMethod: unknown CATEGORY {}", cat),
                }
                .into());
            }
            None => {
                println!("CompuMethod::from_xml: missing CATEGORY"); // todo...
                return Err(FibexError::new("CompuMethod: missing CATEGORY").into());
            }
        };
        let internal_to_phys_scales = xml_e
            .child_by_name("COMPU-INTERNAL-TO-PHYS")
            .and_then(|c| c.child_by_name("COMPU-SCALES"))
            .map(|s| {
                s.children
                    .iter()
                    .filter_map(|s| CompuScale::from_xml(s).ok())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        Ok(CompuMethod {
            category,
            internal_to_phys_scales,
        })
    }
}

impl CompuScale {
    fn from_xml(xml_e: &XmlElement) -> Result<CompuScale, Box<dyn Error>> {
        let mask = xml_e
            .child_by_name("MASK")
            .and_then(|m| m.text.as_deref())
            .and_then(|t| u64::from_str_radix(t, 2).ok());
        let compu_const = xml_e
            .child_by_name("COMPU-CONST")
            .and_then(|c| {
                if c.children.len() == 1 {
                    Some(&c.children[0])
                } else {
                    None
                }
            })
            .map(|c| (c.name.as_str(), c.text.as_deref().unwrap_or_default()))
            .and_then(|(name, text)| match name {
                "V" => Some(VvT::V(text.into())),
                //"V" =>  text.parse::<f64>().ok().map(|v|VvT::V(v)),
                "VT" => Some(VvT::VT(text.to_owned())),
                _ => None,
            });
        // <ho:LOWER-LIMIT INTERVAL-TYPE="CLOSED">1048576</ho:LOWER-LIMIT>
        let lower_limit = xml_e.child_by_name("LOWER-LIMIT").map(IntervalType::from);
        let upper_limit = xml_e.child_by_name("LOWER-LIMIT").map(IntervalType::from);

        Ok(CompuScale {
            mask,
            lower_limit,
            upper_limit,
            compu_const,
        })
    }

    /// if the CompuScale represents a single value return that one else return None
    /// 
    /// A CompuScale represents a single value if:
    ///   - lower_limit == upper_limit 
    ///   - lower_limit type of INCLUDED
    pub fn get_single_value(&self) -> Option<&XsDouble> {
        if let Some(lower_limit) = &self.lower_limit {
            if let std::ops::Bound::Included(lv) = &lower_limit.0 {
                if let Some(upper_limit) = &self.upper_limit {
                    if let std::ops::Bound::Included(uv) = &upper_limit.0 {
                        if uv == lv {
                            return Some(uv);
                        }
                    }
                }
            }
        }
        None
    }
}

impl CodedType {
    fn from_xml(xml_e: &XmlElement) -> Result<CodedType, Box<dyn Error>> {
        // todo optimize performance by avoiding XmlElement!
        let category: Category = if let Some((_k, v)) = xml_e.attr("CATEGORY") {
            match v.as_str() {
                "STANDARD-LENGTH-TYPE" => Category::StandardLengthType,
                "LEADING-LENGTH-INFO-TYPE" => Category::LeadingLengthInfoType,
                "MIN-MAX-LENGTH-TYPE" => Category::MinMaxLengthType,
                _ => return Err(FibexError::new("Invalid category for CodedType").into()),
            }
        } else {
            return Err(FibexError::new("CATEGORY missing for CodedType").into());
        };

        let base_data_type = if let Some((_k, v)) = xml_e.attr("BASE-DATA-TYPE") {
            Some(match v.as_str() {
                "A_UINT8" => BaseDataType::AUint8,
                "A_INT8" => BaseDataType::AInt8,
                "A_UINT16" => BaseDataType::AUint16,
                "A_INT16" => BaseDataType::AInt16,
                "A_UINT32" => BaseDataType::AUint32,
                "A_INT32" => BaseDataType::AInt32,
                "A_UINT64" => BaseDataType::AUint64,
                "A_INT64" => BaseDataType::AInt64,
                "A_FLOAT32" => BaseDataType::AFloat32,
                "A_FLOAT64" => BaseDataType::AFloat64,
                "A_ASCIISTRING" => BaseDataType::AAsciiString,
                "A_UNICODE2STRING" => BaseDataType::AUnicode2String,
                "A_BYTEFIELD" => BaseDataType::AByteField,
                "A_BITFIELD" => BaseDataType::ABitField,
                _ => BaseDataType::Other, // todo or more strict on only OTHER and err?
            })
        } else {
            None
        };

        let termination = if let Some((_k, v)) = xml_e.attr("TERMINATION") {
            match v.as_str() {
                "NONE" => Some(HoTermination::None),
                "ZERO" => Some(HoTermination::Zero),
                "HEX-FF" => Some(HoTermination::HexFF),
                "LENGTH" => Some(HoTermination::Length),
                _ => None, // todo or more strict on only OTHER and err?
            }
        } else {
            None
        };

        let encoding = if let Some((_k, v)) = xml_e.attr("ENCODING") {
            match v.as_str() {
                "UTF-8" => Some(Encoding::Utf8),
                "UTF-16" => Some(Encoding::Utf16),
                "UCS-2" => Some(Encoding::Ucs2),
                "ISO-8859-1" => Some(Encoding::Iso8859_1),
                "ISO-8859-2" => Some(Encoding::Iso8859_2),
                "WINDOWS-1252" => Some(Encoding::Windows1252),
                "SIGNED" => Some(Encoding::Signed),
                "UNSIGNED" => Some(Encoding::Unsigned),
                "BIT" => Some(Encoding::Bit),
                "BCD" => Some(Encoding::Bcd),
                "BCD-P" => Some(Encoding::BcdP),
                "BCD-UP" => Some(Encoding::BcdUp),
                "SM" => Some(Encoding::SM),
                "1C" => Some(Encoding::E1C),
                "2C" => Some(Encoding::E2C),
                "IEEE-FLOATING-TYPE" => Some(Encoding::IeeeFloating),
                "DSP-FRACTIONAL" => Some(Encoding::DspFractional),
                _ => None, // todo or error?
            }
        } else {
            None
        };

        let bit_length = xml_e
            .child_by_name("BIT-LENGTH")
            .and_then(|bl| bl.text.as_ref().and_then(|bl| bl.parse::<u32>().ok()));
        let min_length = xml_e
            .child_by_name("MIN-LENGTH")
            .and_then(|bl| bl.text.as_ref().and_then(|bl| bl.parse::<u32>().ok()));
        let max_length = xml_e
            .child_by_name("MAX-LENGTH")
            .and_then(|bl| bl.text.as_ref().and_then(|bl| bl.parse::<u32>().ok()));

        Ok(CodedType {
            base_data_type,
            category,
            bit_length,
            min_length,
            max_length,
            encoding,
            termination,
        })
    }
}

impl Default for FibexData {
    fn default<'a>() -> Self {
        Self::new()
    }
}

/// load all fibex specified into a single FibexData result
pub fn load_all_fibex(files: &[PathBuf]) -> Result<FibexData, FibexError> {
    let mut fd = FibexData::new();

    for file in files {
        if let Err(e) = fd.load_fibex_file(file) {
            println!("load_fibex_file(file={:?}) failed with:{}", file, e);
        }
    }
    Ok(fd)
}

/// determine all fibex files in one dir
///
/// for now this is equivalent to all .xml files in the sub dir
///
/// Search can be recursive. In that case all non symlink dir entries will be searched as well.
/// io::errors from sub dirs are ignored
pub fn get_all_fibex_in_dir(dir: &Path, recursive: bool) -> Result<Vec<PathBuf>, std::io::Error> {
    let entries = dir.read_dir()?;
    let mut res = Vec::new();
    for entry in entries.flatten() {
        if entry.path().is_dir() {
            if recursive && !entry.path().is_symlink() {
                // dont recurse into symlinks
                let sub = get_all_fibex_in_dir(&entry.path(), true);
                if let Ok(sub) = sub {
                    for p in sub {
                        res.push(p);
                    }
                } // we ignore errs from sub dirs.
            }
        } else if entry.path().is_file() {
            if let Some(ext) = entry.path().extension() {
                if ext.eq_ignore_ascii_case("xml") {
                    res.push(entry.path().clone());
                }
            }
        }
    }
    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn non_dotted_name() {
        assert_eq!(XmlElement::non_dotted_name("foo"), "foo");
        assert_eq!(XmlElement::non_dotted_name(":foo"), "foo");
        assert_eq!(XmlElement::non_dotted_name(":"), "");
        assert_eq!(XmlElement::non_dotted_name(""), "");
        assert_eq!(XmlElement::non_dotted_name("ho:"), "");
        assert_eq!(XmlElement::non_dotted_name("ho:OID"), "OID");
        assert_eq!(XmlElement::non_dotted_name("ho:OID:ID"), "ID"); // last dot
    }

    #[test]
    fn load_fibex1() {
        let mut fb = FibexData::new();
        let path = Path::new("tests/fibex1.xml");
        assert!(path.exists());
        let r = fb.load_fibex_file(path);
        assert!(r.is_ok(), "{:?}", r.err());
        assert_eq!(fb.pi.codings.len(), 3);
        assert_eq!(fb.elements.datatypes_map_by_id.len(), 5);
        let dt = &fb
            .elements
            .datatypes_map_by_id
            .get("de_mbehr_testservices_TestService1API_DataStruct")
            .unwrap()
            .datatype;
        if let DatatypeType::ComplexType(cdt) = dt {
            assert!(cdt.members.len() == 2)
        } else {
            panic!()
        }

        println!("fb={:?}", fb);
    }
}
