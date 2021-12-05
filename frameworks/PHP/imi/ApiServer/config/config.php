<?php
return [
    'configs'    =>    [
    ],
    'beans'    =>    [
        'HttpDispatcher'    =>    [
            'middleware' => false,
        ],
        'HtmlView'    =>    [
            'templatePath'    =>    dirname(__DIR__) . '/template/',
            // 支持的模版文件扩展名，优先级按先后顺序
            'fileSuffixs'        =>    [
                'tpl',
                'html',
                'php'
            ],
        ]
    ],
];