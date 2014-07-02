<@compress single_line=true>
[<#list worlds as w>
{"id":${w.id},"randomNumber":${w.randomNumber}}
<#if w_has_next>,</#if>
</#list>
]
</@compress>