<?php

return [
    'message' => [
        'required'      => 'The :attribute is mandatory' ,
        'string'        => 'The :attribute must be a string' ,
        'integer'       => 'The :attribute must be a number' ,
        'boolean'       => 'The :attribute must be true or false' ,
        'array'         => 'The :attribute must be an array' ,
        'email'         => 'The :attribute must be the email address' ,
        'regex'         => 'The template :attribute is wrong' ,
        'notRegex'      => 'The template :attribute is wrong' ,
        'max'           => 'The :attribute field should not be greater than :value' ,
        'min'           => 'The :attribute field should not be less than :value' ,
        'size'          => 'The field :attribute must be equal to :value' ,
        'after'         => 'The :attribute field must be larger than the :value field' ,
        'before'        => 'The :attribute field must be smaller than the :value field' ,
        'in'            => 'The field :attribute must be equal to one of the values :value' ,
        'date'          => 'The :attribute must be of date type' ,
        'exists'        => 'Such :attribute does not exist' ,
        'unique'        => 'Such :attribute exists' ,
        'nationalCode'  => 'The national code entered in the :attribute field is incorrect'
    ],

    'attribute' => [
        'firstName'     => 'first name' ,
        'lastName'      => 'last name' ,
        'phone'         => 'phone'
    ]
];