<?php

declare(strict_types=1);

return [
    /*
    |--------------------------------------------------------------------------
    | Validation Language Lines
    |--------------------------------------------------------------------------
    |
    | The following language lines contain the default error messages used by
    | the validator class. Some of these rules have multiple versions such
    | as the size rules. Feel free to tweak each of these messages here.
    |
    */

    'accepted' => ':attribute 必须接受',
    'active_url' => ':attribute 必须是一个合法的 URL',
    'after' => ':attribute 必须是 :date 之后的一个日期',
    'after_or_equal' => ':attribute 必须是 :date 之后或相同的一个日期',
    'alpha' => ':attribute 只能包含字母',
    'alpha_dash' => ':attribute 只能包含字母、数字、中划线或下划线',
    'alpha_num' => ':attribute 只能包含字母和数字',
    'array' => ':attribute 必须是一个数组',
    'before' => ':attribute 必须是 :date 之前的一个日期',
    'before_or_equal' => ':attribute 必须是 :date 之前或相同的一个日期',
    'between' => [
        'numeric' => ':attribute 必须在 :min 到 :max 之间',
        'file' => ':attribute 必须在 :min 到 :max kb 之间',
        'string' => ':attribute 必须在 :min 到 :max 个字符之间',
        'array' => ':attribute 必须在 :min 到 :max 项之间',
    ],
    'boolean' => ':attribute 字符必须是 true 或 false, 1 或 0',
    'confirmed' => ':attribute 二次确认不匹配',
    'date' => ':attribute 必须是一个合法的日期',
    'date_format' => ':attribute 与给定的格式 :format 不符合',
    'decimal' => ':attribute 必须有 :decimal 位小数',
    'different' => ':attribute 必须不同于 :other',
    'digits' => ':attribute 必须是 :digits 位',
    'digits_between' => ':attribute 必须在 :min 和 :max 位之间',
    'dimensions' => ':attribute 具有无效的图片尺寸',
    'distinct' => ':attribute 字段具有重复值',
    'email' => ':attribute 必须是一个合法的电子邮件地址',
    'exists' => '选定的 :attribute 是无效的',
    'file' => ':attribute 必须是一个文件',
    'filled' => ':attribute 的字段是必填的',
    'gt' => [
        'numeric' => ':attribute 必须大于 :value',
        'file' => ':attribute 必须大于 :value kb',
        'string' => ':attribute 必须大于 :value 个字符',
        'array' => ':attribute 必须大于 :value 项',
    ],
    'gte' => [
        'numeric' => ':attribute 必须大于等于 :value',
        'file' => ':attribute 必须大于等于 :value kb',
        'string' => ':attribute 必须大于等于 :value 个字符',
        'array' => ':attribute 必须大于等于 :value 项',
    ],
    'image' => ':attribute 必须是 jpg, jpeg, png, bmp 或者 gif 格式的图片',
    'in' => '选定的 :attribute 是无效的',
    'in_array' => ':attribute 字段不存在于 :other',
    'integer' => ':attribute 必须是个整数',
    'ip' => ':attribute 必须是一个合法的 IP 地址',
    'ipv4' => ':attribute 必须是一个合法的 IPv4 地址',
    'ipv6' => ':attribute 必须是一个合法的 IPv6 地址',
    'json' => ':attribute 必须是一个合法的 JSON 字符串',
    'list' => ':attribute 必须是一个数组列表',
    'lt' => [
        'numeric' => ':attribute 必须小于 :value',
        'file' => ':attribute 必须小于 :value kb',
        'string' => ':attribute 必须小于 :value 个字符',
        'array' => ':attribute 必须小于 :value 项',
    ],
    'lte' => [
        'numeric' => ':attribute 必须小于等于 :value',
        'file' => ':attribute 必须小于等于 :value kb',
        'string' => ':attribute 必须小于等于 :value 个字符',
        'array' => ':attribute 必须小于等于 :value 项',
    ],
    'max' => [
        'numeric' => ':attribute 的最大值为 :max',
        'file' => ':attribute 的最大为 :max kb',
        'string' => ':attribute 的最大长度为 :max 字符',
        'array' => ':attribute 至多有 :max 项',
    ],
    'mimes' => ':attribute 的文件类型必须是 :values',
    'mimetypes' => ':attribute 的文件MIME必须是 :values',
    'min' => [
        'numeric' => ':attribute 的最小值为 :min',
        'file' => ':attribute 大小至少为 :min kb',
        'string' => ':attribute 的最小长度为 :min 字符',
        'array' => ':attribute 至少有 :min 项',
    ],
    'not_in' => '选定的 :attribute 是无效的',
    'not_regex' => ':attribute 不能匹配给定的正则',
    'numeric' => ':attribute 必须是数字',
    'present' => ':attribute 字段必须存在',
    'prohibits' => '必须提供 :attribute 字段',
    'regex' => ':attribute 格式是无效的',
    'required' => ':attribute 字段是必须的',
    'required_if' => ':attribute 字段是必须的当 :other 是 :value',
    'required_unless' => ':attribute 字段是必须的，除非 :other 是在 :values 中',
    'required_with' => ':attribute 字段是必须的当 :values 是存在的',
    'required_with_all' => ':attribute 字段是必须的当 :values 是存在的',
    'required_without' => ':attribute 字段是必须的当 :values 是不存在的',
    'required_without_all' => ':attribute 字段是必须的当 没有一个 :values 是存在的',
    'exclude' => ':attribute 字段是被排除的',
    'exclude_if' => '当 :other 为 :value 时，排除 :attribute 字段',
    'exclude_unless' => '除非 :other 是在 :values 中，否则排除 :attribute 字段',
    'exclude_with' => '当 :values 存在时，排除 :attribute 字段',
    'exclude_without' => '当 :values 不存在时，排除 :attribute 字段',
    'same' => ':attribute 和 :other 必须匹配',
    'size' => [
        'numeric' => ':attribute 必须是 :size',
        'file' => ':attribute 必须是 :size kb',
        'string' => ':attribute 必须是 :size 个字符',
        'array' => ':attribute 必须包括 :size 项',
    ],
    'starts_with' => ':attribute 必须以 :values 为开头',
    'string' => ':attribute 必须是一个字符串',
    'timezone' => ':attribute 必须是个有效的时区',
    'unique' => ':attribute 已存在',
    'uploaded' => ':attribute 上传失败',
    'url' => ':attribute 无效的格式',
    'uuid' => ':attribute 无效的UUID格式',
    'max_if' => [
        'numeric' => '当 :other 为 :value 时 :attribute 不能大于 :max',
        'file' => '当 :other 为 :value 时 :attribute 不能大于 :max kb',
        'string' => '当 :other 为 :value 时 :attribute 不能大于 :max 个字符',
        'array' => '当 :other 为 :value 时 :attribute 最多只有 :max 个单元',
    ],
    'min_if' => [
        'numeric' => '当 :other 为 :value 时 :attribute 必须大于等于 :min',
        'file' => '当 :other 为 :value 时 :attribute 大小不能小于 :min kb',
        'string' => '当 :other 为 :value 时 :attribute 至少为 :min 个字符',
        'array' => '当 :other 为 :value 时 :attribute 至少有 :min 个单元',
    ],
    'between_if' => [
        'numeric' => '当 :other 为 :value 时 :attribute 必须介于 :min - :max 之间',
        'file' => '当 :other 为 :value 时 :attribute 必须介于 :min - :max kb 之间',
        'string' => '当 :other 为 :value 时 :attribute 必须介于 :min - :max 个字符之间',
        'array' => '当 :other 为 :value 时 :attribute 必须只有 :min - :max 个单元',
    ],
    /*
    |--------------------------------------------------------------------------
    | Custom Validation Language Lines
    |--------------------------------------------------------------------------
    |
    | Here you may specify custom validation messages for attributes using the
    | convention "attribute.rule" to name the lines. This makes it quick to
    | specify a specific custom language line for a given attribute rule.
    |
    */

    'custom' => [
        'attribute-name' => [
            'rule-name' => 'custom-message',
        ],
    ],

    /*
    |--------------------------------------------------------------------------
    | Custom Validation Attributes
    |--------------------------------------------------------------------------
    |
    | The following language lines are used to swap attribute place-holders
    | with something more reader friendly such as E-Mail Address instead
    | of "email". This simply helps us make messages a little cleaner.
    |
    */

    'attributes' => [],
    'phone_number' => ':attribute 必须为一个有效的电话号码',
    'telephone_number' => ':attribute 必须为一个有效的手机号码',

    'chinese_word' => ':attribute 必须包含以下有效字符 (中文/英文，数字, 下划线)',
    'sequential_array' => ':attribute 必须是一个有序数组',
];
