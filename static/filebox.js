$(function(){
    function zeroPadDate(date){
        return ("0" + date).slice(-2);
    }

    function formatDate(date){
        return date.getFullYear()+"."+
            zeroPadDate(date.getUTCMonth()+1)+"."+
            zeroPadDate(date.getUTCDate())+" "+
            zeroPadDate(date.getUTCHours())+":"+
            zeroPadDate(date.getUTCMinutes())+":"+
            zeroPadDate(date.getUTCSeconds());
    }
    
    function always(thing, matchers){
        for(var i in matchers){
            if(!matchers[i](thing)){
                return false;
            }
        }
        return true;
    }

    function contains(string, search){
        return (string.toUpperCase().indexOf(search.toUpperCase()) > -1);
    }
    
    function compileMatcher(string){
        console.log("Compiling Matcher", string);
        var parts = string.split(" ");
        $.each(parts, function(i, val){
            if(contains(val, ":")){
                val = val.split(":");
                var match = val[1];
                var field = val[0].toUpperCase();
                parts[i] = function(obj){
                    return (contains(obj[field], match));
                };
            }else{
                var match = val;
                parts[i] = function(obj){
                    for(var key in obj){
                        if(contains(obj[key], match))
                            return true;
                    }
                    return false;
                };
            }
        });
        return parts;
    }
    
    function setupFilterableList(list, filter){
        console.log("Setting up filter for",list,"using",filter);
        
        function entryToObject(entry){
            var attributes = $(".attrs", entry).text().split(" ");
            var object = {};
            object["NAME"]=$(".file", entry).text();
            object["TYPE"]=$(".type", entry).text();
            object["TIME"]=$("time", entry).text();
            object["*catchall*"]="";
            $.each(attributes, function(i, val){
                if(contains(val, ":")){
                    var keyval = val.split(":");
                    object[keyval[0].toUpperCase()]=keyval[1];
                }else{
                    object["*catchall*"]=object["*catchall*"]+val;
                }
            });
            return object;
        }
        
        function refilter(matchers){
            $(">li", list).each(function(i, el){
                if(always(entryToObject(el), matchers)){
                    $(this).show();
                }else{
                    $(this).hide();
                }
            });
        }

        $(filter).on('input', function(){
            refilter(compileMatcher($(this).val()));
        });
    }

    function maybeMakeClickable(item){
        var type = $(".type",item).text();
        var file = $(".file",item).attr("href");
        var prev = $(".preview",item);
        if(type == "image/png" ||
           type == "image/gif" ||
           type == "image/bmp" ||
           type == "image/jpg" ||
           type == "image/jpeg" ||
           type == "image/svg+xml"){
            $(item).click(function(){
                if(prev.css("display") === "none"){
                    if($("img",prev).length == 0){
                        var img = document.createElement("img");
                        $(img).attr({"src":file,
                                     "alt":"preview"})
                            .appendTo($("a",prev));
                    }
                    $(prev).css("display", "block");
                }else{
                    $(prev).css("display", "none");
                }
            });
        }
    }

    function clickableAttributes(){
        // Gather attributes, group together.
        var attributes = {};
        $("#files .attrs").each(function(){
            var inner = $(this).text().split(" ");
            $.each(inner, function(i, val){
                if(val !== ""){
                    if(attributes[val] == undefined){
                        attributes[val] = 1;
                    }else{
                        attributes[val]++;
                    }
                }
            });
        });
        // Sort by frequency.
        var sorted = [];
        for(var attr in attributes){
            sorted.push([attr, attributes[attr]]);
        }
        sorted.sort(function(a,b){return b[1]-a[1];});
        // Generate elements
        var list = document.createElement("ul");
        $.each(sorted, function(i, val){
            val = val[0];
            var attr = document.createElement("li");
            $(attr).text(val)
                .appendTo(list)
                .click(function(){
                    var current = $("#upload .attrs").val();
                    if(contains(current, val)){
                        $("#upload .attrs").val(current.replace(new RegExp("\\s*"+val+"\\s*","g")," "));
                    }else{
                        $("#upload .attrs").val(current+" "+val);
                    }
                });
        });
        // Publish list
        $(list).attr({"class":"interactive-attrs",
                      "title":"This is a list of attributes generated from what you already used. Click on one to add/remove it to/from the attribute input field."})
            .appendTo("#upload");
    }

    function copyToClipboard(content){
        navigator.clipboard.writeText(content).then(function(){
            var notice = document.querySelector("#notice");
            if(!notice){
                notice = document.createElement("div");
                notice.setAttribute("id","notice");
                document.querySelector("body>header").appendChild(notice);
            }
            notice.innerText = "URL Copied to clipboard.";
        }, function(){
            console.log("Failed to copy", content, "to clipboard");
        });
    }

    // Check if we have an upload notice and copy.
    if(document.querySelector("#notice a")){
        copyToClipboard(document.querySelector("#notice a").getAttribute("href"));
    }
    
    // allow filtering
    setupFilterableList($("#files"), $("#filter"));

    // upload dummy timestamp
    $("#upload time").text(formatDate(new Date()));

    // remove empty attrs
    $("#files li .attrs").each(function(){
        if($(this).text().trim()===""){
            $(this).remove();
        }
    });

    // previews
    $("#files li").each(function(){
        maybeMakeClickable(this);
    });

    // more convenient attributes
    clickableAttributes();
});
