gitbook.require(["jQuery"],function(jQuery){jQuery.extend({highlight:function(node,re,nodeName,className){if(3===node.nodeType){var match=node.data.match(re);if(match){var highlight=document.createElement(nodeName||"span");highlight.className=className||"highlight";var wordNode=node.splitText(match.index);wordNode.splitText(match[0].length);var wordClone=wordNode.cloneNode(!0);return highlight.appendChild(wordClone),wordNode.parentNode.replaceChild(highlight,wordNode),1}}else if(1===node.nodeType&&node.childNodes&&!/(script|style)/i.test(node.tagName)&&(node.tagName!==nodeName.toUpperCase()||node.className!==className))for(var i=0;i<node.childNodes.length;i++)i+=jQuery.highlight(node.childNodes[i],re,nodeName,className);return 0}}),jQuery.fn.unhighlight=function(options){var settings={className:"highlight",element:"span"};return jQuery.extend(settings,options),this.find(settings.element+"."+settings.className).each(function(){var parent=this.parentNode;parent.replaceChild(this.firstChild,this),parent.normalize()}).end()},jQuery.fn.highlight=function(words,options){var settings={className:"highlight",element:"span",caseSensitive:!1,wordsOnly:!1};if(jQuery.extend(settings,options),words.constructor===String&&(words=[words],/\s/.test(words[0])&&words.push(words[0].replace(/\s+/,"-"))),words=jQuery.grep(words,function(word){return""!==word}),words=jQuery.map(words,function(word){return word.replace(/[-[\]{}()*+?.,\\^$|#\s]/g,"\\$&")}),0===words.length)return this;var flag=settings.caseSensitive?"":"i",pattern="("+words.join("|")+")";settings.wordsOnly&&(pattern="\\b"+pattern+"\\b");var re=new RegExp(pattern,flag);return this.each(function(){jQuery.highlight(this,re,settings.element,settings.className)})}});
