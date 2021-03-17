<?php

use Phalcon\Mvc\Model;
use Phalcon\Mvc\Model\MetaData;
use Phalcon\Db\Column;

class Fortunes extends Model
{
    public $id;

    public $message;

    public function initialize()
    {
        $this->setSource('Fortune');
    }

    public function metaData(): array
    {
        return [
            // Every column in the mapped table
            MetaData::MODELS_ATTRIBUTES => [
                'id',
                'message'
            ],

            // Every column part of the primary key
            MetaData::MODELS_PRIMARY_KEY => [
                'id',
            ],

            // Every column that isn't part of the primary key
            MetaData::MODELS_NON_PRIMARY_KEY => [
                'message'
            ],

            // Every column that doesn't allows null values
            MetaData::MODELS_NOT_NULL => [
                'id',
                'message'
            ],

            // Every column and their data types
            MetaData::MODELS_DATA_TYPES => [
                'id' => Column::TYPE_INTEGER,
                'message' => Column::TYPE_VARCHAR
            ],

            // The columns that have numeric data types
            MetaData::MODELS_DATA_TYPES_NUMERIC => [
                'id' => true
            ],

            // The identity column, use boolean false if the model doesn't have
            // an identity column
            MetaData::MODELS_IDENTITY_COLUMN => 'id',

            // How every column must be bound/casted
            MetaData::MODELS_DATA_TYPES_BIND => [
                'id' => Column::BIND_PARAM_INT,
                'message' => Column::BIND_PARAM_STR
            ],

            // Fields that must be ignored from INSERT SQL statements
            MetaData::MODELS_AUTOMATIC_DEFAULT_INSERT => [],

            // Fields that must be ignored from UPDATE SQL statements
            MetaData::MODELS_AUTOMATIC_DEFAULT_UPDATE => [],

            // Default values for columns
            MetaData::MODELS_DEFAULT_VALUES => [],

            // Fields that allow empty strings
            MetaData::MODELS_EMPTY_STRING_VALUES => [],
        ];
    }
}
