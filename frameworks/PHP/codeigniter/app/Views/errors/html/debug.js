var tabLinks    = new Array();
var contentDivs = new Array();

function init()
{
    // Grab the tab links and content divs from the page
    var tabListItems = document.getElementById('tabs').childNodes;
    console.log(tabListItems);
    for (var i = 0; i < tabListItems.length; i ++)
    {
        if (tabListItems[i].nodeName == "LI")
        {
            var tabLink     = getFirstChildWithTagName(tabListItems[i], 'A');
            var id          = getHash(tabLink.getAttribute('href'));
            tabLinks[id]    = tabLink;
            contentDivs[id] = document.getElementById(id);
        }
    }

    // Assign onclick events to the tab links, and
    // highlight the first tab
    var i = 0;

    for (var id in tabLinks)
    {
        tabLinks[id].onclick = showTab;
        tabLinks[id].onfocus = function () {
            this.blur()
        };
        if (i == 0)
        {
            tabLinks[id].className = 'active';
        }
        i ++;
    }

    // Hide all content divs except the first
    var i = 0;

    for (var id in contentDivs)
    {
        if (i != 0)
        {
            console.log(contentDivs[id]);
            contentDivs[id].className = 'content hide';
        }
        i ++;
    }
}

function showTab()
{
    var selectedId = getHash(this.getAttribute('href'));

    // Highlight the selected tab, and dim all others.
    // Also show the selected content div, and hide all others.
    for (var id in contentDivs)
    {
        if (id == selectedId)
        {
            tabLinks[id].className    = 'active';
            contentDivs[id].className = 'content';
        }
        else
        {
            tabLinks[id].className    = '';
            contentDivs[id].className = 'content hide';
        }
    }

    // Stop the browser following the link
    return false;
}

function getFirstChildWithTagName(element, tagName)
{
    for (var i = 0; i < element.childNodes.length; i ++)
    {
        if (element.childNodes[i].nodeName == tagName)
        {
            return element.childNodes[i];
        }
    }
}

function getHash(url)
{
    var hashPos = url.lastIndexOf('#');
    return url.substring(hashPos + 1);
}

function toggle(elem)
{
    elem = document.getElementById(elem);

    if (elem.style && elem.style['display'])
    {
        // Only works with the "style" attr
        var disp = elem.style['display'];
    }
    else if (elem.currentStyle)
    {
        // For MSIE, naturally
        var disp = elem.currentStyle['display'];
    }
    else if (window.getComputedStyle)
    {
        // For most other browsers
        var disp = document.defaultView.getComputedStyle(elem, null).getPropertyValue('display');
    }

    // Toggle the state of the "display" style
    elem.style.display = disp == 'block' ? 'none' : 'block';

    return false;
}
