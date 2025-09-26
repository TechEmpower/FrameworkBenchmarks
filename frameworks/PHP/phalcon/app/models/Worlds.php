<?php

use Phalcon\Mvc\Model;
use Phalcon\Mvc\Model\MetaData;
use Phalcon\Db\Column;

class Worlds extends Model
{
    public $id;

    public $randomNumber;

    public function initialize()
    {
        $this->setSource('World');
    }

    public function metaData(): array
    {
        return [
            // Every column in the mapped table
            MetaData::MODELS_ATTRIBUTES => [
                'id',
                'randomNumber'
            ],

            // Every column part of the primary key
            MetaData::MODELS_PRIMARY_KEY => [
                'id',
            ],

            // Every column that isn't part of the primary key
            MetaData::MODELS_NON_PRIMARY_KEY => [
                'randomNumber'
            ],

            // Every column that doesn't allows null values
            MetaData::MODELS_NOT_NULL => [
                'id',
                'randomNumber'
            ],

            // Every column and their data types
            MetaData::MODELS_DATA_TYPES => [
                'id' => Column::TYPE_INTEGER,
                'randomNumber' => Column::TYPE_INTEGER
            ],

            // The columns that have numeric data types
            MetaData::MODELS_DATA_TYPES_NUMERIC => [
                'id' => true,
                'randomNumber' => true,
            ],

            // The identity column, use boolean false if the model doesn't have
            // an identity column
            MetaData::MODELS_IDENTITY_COLUMN => 'id',

            // How every column must be bound/casted
            MetaData::MODELS_DATA_TYPES_BIND => [
                'id' => Column::BIND_PARAM_INT,
                'randomNumber' => Column::BIND_PARAM_INT
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