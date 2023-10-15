use std::any::Any;

#[derive(Debug, PartialEq, Eq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    ReturnValue,
    Error,
}

impl ToString for ObjectType {
    fn to_string(&self) -> String {
        match self {
            ObjectType::Integer => "INTEGER".to_string(),
            ObjectType::Boolean => "BOOLEAN".to_string(),
            ObjectType::Null => "NULL".to_string(),
            ObjectType::ReturnValue => "RETURN_VALUE".to_string(),
            ObjectType::Error => "ERROR".to_string(),
        }
    }
}

pub trait Object {
    fn type_name(&self) -> ObjectType;
    fn to_string(&self) -> String;
    fn inspect(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn type_name(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }

    fn inspect(&self) -> String {
        self.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn type_name(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }

    fn inspect(&self) -> String {
        self.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Null;

impl Object for Null {
    fn type_name(&self) -> ObjectType {
        ObjectType::Null
    }

    fn to_string(&self) -> String {
        "null".to_string()
    }

    fn inspect(&self) -> String {
        self.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct ReturnValue {
    pub value: Box<dyn Object>,
}

impl Object for ReturnValue {
    fn type_name(&self) -> ObjectType {
        ObjectType::ReturnValue
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ReturnValue {
    pub fn get_return_value(&self) -> Box<dyn Object> {
        if let Some(integer) = self.value.as_any().downcast_ref::<Integer>() {
            return Box::new(Integer {
                value: integer.value,
            });
        }
        if let Some(boolean) = self.value.as_any().downcast_ref::<Boolean>() {
            return Box::new(Boolean {
                value: boolean.value,
            });
        }
        if self.value.as_any().downcast_ref::<Null>().is_some() {
            return Box::new(Null {});
        }
        if let Some(return_value) = self.value.as_any().downcast_ref::<ReturnValue>() {
            return return_value.get_return_value();
        }

        if let Some(error) = self.value.as_any().downcast_ref::<Error>() {
            return error.into();
        }
        unreachable!(
            "Could not get return value from object ({}): {}",
            self.type_name().to_string(),
            self.to_string()
        )
    }
}

pub struct Error {
    pub message: String,
}

impl Error {
    pub fn new(message: String) -> Self {
        Error { message }
    }

    pub fn is_error(obj: &dyn Object) -> bool {
        obj.type_name() == ObjectType::Error
    }
}

impl Object for Error {
    fn type_name(&self) -> ObjectType {
        ObjectType::ReturnValue
    }

    fn to_string(&self) -> String {
        format!("ERROR: {}", self.message)
    }

    fn inspect(&self) -> String {
        self.to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl From<&Error> for Box<dyn Object> {
    fn from(error: &Error) -> Self {
        Box::new(Error {
            message: error.message.clone(),
        })
    }
}
