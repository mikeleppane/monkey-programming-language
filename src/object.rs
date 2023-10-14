use std::any::Any;

pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

impl ToString for ObjectType {
    fn to_string(&self) -> String {
        match self {
            ObjectType::Integer => "INTEGER".to_string(),
            ObjectType::Boolean => "BOOLEAN".to_string(),
            ObjectType::Null => "NULL".to_string(),
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
