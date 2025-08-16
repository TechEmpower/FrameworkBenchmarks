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

    'accepted' => '必須接受 :attribute',
    'active_url' => ':attribute 必須是有效的網址',
    'after' => ':attribute 必須是 :date 之後的日期',
    'after_or_equal' => ':attribute 必須是 :date 之後或相同的日期',
    'alpha' => ':attribute 只能包含字母',
    'alpha_dash' => ':attribute 只能包含字母、數字、連接線或底線',
    'alpha_num' => ':attribute 只能包含字母和數字',
    'array' => ':attribute 必須是陣列',
    'before' => ':attribute 必須是 :date 之前的日期',
    'before_or_equal' => ':attribute 必須是 :date 之前或相同的日期',
    'between' => [
        'numeric' => ':attribute 必須介於 :min 至 :max 之間',
        'file' => ':attribute 必須介於 :min 至 :max KB 之間',
        'string' => ':attribute 必須介於 :min 至 :max 個字元之間',
        'array' => ':attribute 必須有 :min 至 :max 個項目',
    ],
    'boolean' => ':attribute 欄位必須是 true 或 false、1 或 0',
    'confirmed' => ':attribute 確認欄位不相符',
    'date' => ':attribute 必須是有效的日期',
    'date_format' => ':attribute 與格式 :format 不符',
    'decimal' => ':attribute 必須有 :decimal 位小數',
    'different' => ':attribute 與 :other 必須不同',
    'digits' => ':attribute 必須是 :digits 位數字',
    'digits_between' => ':attribute 必須介於 :min 至 :max 位數字',
    'dimensions' => ':attribute 圖片尺寸不正確',
    'distinct' => ':attribute 已經存在',
    'email' => ':attribute 必須是有效的電子郵件位址',
    'exists' => '所選擇的 :attribute 無效',
    'file' => ':attribute 必須是檔案',
    'filled' => ':attribute 不能留空',
    'gt' => [
        'numeric' => ':attribute 必須大於 :value',
        'file' => ':attribute 必須大於 :value KB',
        'string' => ':attribute 必須多於 :value 個字元',
        'array' => ':attribute 必須多於 :value 個項目',
    ],
    'gte' => [
        'numeric' => ':attribute 必須大於或等於 :value',
        'file' => ':attribute 必須大於或等於 :value KB',
        'string' => ':attribute 必須多於或等於 :value 個字元',
        'array' => ':attribute 必須多於或等於 :value 個項目',
    ],
    'image' => ':attribute 必須是 jpg、jpeg、png、bmp 或 gif 格式的圖片',
    'in' => '所選的 :attribute 無效',
    'in_array' => ':attribute 欄位不存在於 :other',
    'integer' => ':attribute 必須是整數',
    'ip' => ':attribute 必須是有效的 IP 位址',
    'ipv4' => ':attribute 必須是有效的 IPv4 位址',
    'ipv6' => ':attribute 必須是有效的 IPv6 位址',
    'json' => ':attribute 必須是有效的 JSON 字串',
    'list' => ':attribute 必須是陣列列表',
    'lt' => [
        'numeric' => ':attribute 必須小於 :value',
        'file' => ':attribute 必須小於 :value KB',
        'string' => ':attribute 必須少於 :value 個字元',
        'array' => ':attribute 必須少於 :value 個項目',
    ],
    'lte' => [
        'numeric' => ':attribute 必須小於或等於 :value',
        'file' => ':attribute 必須小於或等於 :value KB',
        'string' => ':attribute 必須少於或等於 :value 個字元',
        'array' => ':attribute 必須少於或等於 :value 個項目',
    ],
    'max' => [
        'numeric' => ':attribute 不能大於 :max',
        'file' => ':attribute 不能大於 :max KB',
        'string' => ':attribute 不能多於 :max 個字元',
        'array' => ':attribute 最多有 :max 個項目',
    ],
    'mimes' => ':attribute 必須為 :values 的檔案類型',
    'mimetypes' => ':attribute 必須為 :values 的檔案 MIME',
    'min' => [
        'numeric' => ':attribute 不能小於 :min',
        'file' => ':attribute 不能小於 :min KB',
        'string' => ':attribute 不能少於 :min 個字元',
        'array' => ':attribute 至少要有 :min 個項目',
    ],
    'not_in' => '所選擇的 :attribute 無效',
    'not_regex' => ':attribute 的格式錯誤',
    'numeric' => ':attribute 必須為數字',
    'present' => ':attribute 必須存在',
    'prohibits' => '必須提供 :attribute 欄位',
    'regex' => ':attribute 的格式錯誤',
    'required' => ':attribute 不能留空',
    'required_if' => '當 :other 是 :value 時 :attribute 不能留空',
    'required_unless' => '當 :other 不是 :values 時 :attribute 不能留空',
    'required_with' => '當 :values 出現時 :attribute 不能留空',
    'required_with_all' => '當 :values 出現時 :attribute 不能留空',
    'required_without' => '當 :values 留空時 :attribute 不能留空',
    'required_without_all' => '當 :values 都不出現時 :attribute 不能留空',
    'exclude' => ':attribute 欄位被排除',
    'exclude_if' => '當 :other 為 :value 時，排除 :attribute 欄位',
    'exclude_unless' => '除非 :other 在 :values 中，否則排除 :attribute 欄位',
    'exclude_with' => '當 :values 存在時，排除 :attribute 欄位',
    'exclude_without' => '當 :values 不存在時，排除 :attribute 欄位',
    'same' => ':attribute 與 :other 必須相同',
    'size' => [
        'numeric' => ':attribute 必須是 :size',
        'file' => ':attribute 必須是 :size KB',
        'string' => ':attribute 必須是 :size 個字元',
        'array' => ':attribute 必須包含 :size 個項目',
    ],
    'starts_with' => ':attribute 必須以 :values 開頭',
    'string' => ':attribute 必須是字串',
    'timezone' => ':attribute 必須是有效的時區',
    'unique' => ':attribute 已經存在',
    'uploaded' => ':attribute 上傳失敗',
    'url' => ':attribute 格式錯誤',
    'uuid' => ':attribute 必須是有效的 UUID',
    'max_if' => [
        'numeric' => '當 :other 為 :value 時 :attribute 不能大於 :max',
        'file' => '當 :other 為 :value 時 :attribute 不能大於 :max KB',
        'string' => '當 :other 為 :value 時 :attribute 不能多於 :max 個字元',
        'array' => '當 :other 為 :value 時 :attribute 最多只能有 :max 個項目',
    ],
    'min_if' => [
        'numeric' => '當 :other 為 :value 時 :attribute 不能小於 :min',
        'file' => '當 :other 為 :value 時 :attribute 不能小於 :min KB',
        'string' => '當 :other 為 :value 時 :attribute 不能少於 :min 個字元',
        'array' => '當 :other 為 :value 時 :attribute 至少要有 :min 個項目',
    ],
    'between_if' => [
        'numeric' => '當 :other 為 :value 時 :attribute 必須介於 :min 至 :max 之間',
        'file' => '當 :other 為 :value 時 :attribute 必須介於 :min 至 :max KB 之間',
        'string' => '當 :other 為 :value 時 :attribute 必須介於 :min 至 :max 個字元之間',
        'array' => '當 :other 為 :value 時 :attribute 必須有 :min 至 :max 個項目',
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
    'phone_number' => ':attribute 必須是有效的電話號碼',
    'telephone_number' => ':attribute 必須是有效的手機號碼',
    'chinese_word' => ':attribute 必須包含有效的字元（中文/英文、數字、底線）',
    'sequential_array' => ':attribute 必須是有序陣列',
];
