/*
  As of version 1.1.2, Propane will load and execute the contents of
  ~Library/Application Support/Propane/unsupported/caveatPatchor.js
  immediately following the execution of its own enhancer.js file.

  You can use this mechanism to add your own customizations to Campfire
  in Propane.

  Below you'll find two customization examples.

  1 - A responder that adds avatars to your chat view
  2 - A responder that displays CloudApp images inline

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*/

var displayAvatars = true;
var displayCloudAppImages = true;


/* 
  Display avatars in the chat view - based on code originally by @tmm1
*/

if (displayAvatars) {

  Object.extend(Campfire.Message.prototype, {
    addAvatar: function() {
      if (this.actsLikeTextMessage()) {
        var author = this.authorElement();
        var avatar = '';

        if (author.visible()) {
          author.hide();
          if (this.bodyCell.select('strong').length === 0) {
            this.bodyCell.insert({top: '<strong style="color:#333;">'+author.textContent+'</strong><br>'});
            avatar = author.getAttribute('data-avatar') || 'http://asset1.37img.com/global/missing/avatar.png?r=3';
            author.insert({after: '<img alt="'+this.author()+'" width="32" height="32" align="top" style="opacity: 1.0; margin-left: 5px; border-radius:3px" src="'+avatar+'">'});
          }
        }
      }
    }
  });

  /* if you can wrap rather than rewrite, use swizzle like this: */
  swizzle(Campfire.Message, {
    setAuthorVisibilityInRelationTo: function($super, message) {
      $super(message);
      this.addAvatar();
    }
  });


  /* defining a new responder is probably the best way to insulate your hacks from Campfire and Propane */
  Campfire.AvatarMangler = Class.create({
    initialize: function(chat) {
      this.chat = chat;

      var messages = this.chat.transcript.messages;
      for (var i = 0; i < messages.length; i++) {
        var message = messages[i];
        message.addAvatar();
      }

      this.chat.layoutmanager.layout();
      this.chat.windowmanager.scrollToBottom();
    },

    onMessagesInserted: function(messages) {
      var scrolledToBottom = this.chat.windowmanager.isScrolledToBottom();

      for (var i = 0; i < messages.length; i++) {
        var message = messages[i];
        message.addAvatar();
      }

      if (scrolledToBottom) {
        this.chat.windowmanager.scrollToBottom();
      }
    }
  });

  /* Here is how to install your responder into the running chat */
  Campfire.Responders.push("AvatarMangler");
  window.chat.installPropaneResponder("AvatarMangler", "avatarmangler");
}


/* 
  Display CloudApp images inline.

  This responder illustrates using Propane's requestJSON service to request 
  JSON from remote (non-authenticated) servers and have the results passed
  to a callback of your choosing.
*/

if (displayCloudAppImages) {

  Campfire.CloudAppExpander = Class.create({
    initialize: function(chat) {
      this.chat = chat;
      var messages = this.chat.transcript.messages;
      for (var i = 0; i < messages.length; i++) {
        this.detectCloudAppURL(messages[i]);
      }
    },

    detectCloudAppURL: function(message) {
      /* we are going to use the messageID to uniquely identify our requestJSON request
         so we don't check pending messages */
      if (!message.pending() && message.kind === 'text') {
        var links = message.bodyElement().select('a:not(image)');
        if (links.length != 1) {
          return;
        }
        var href = links[0].getAttribute('href');
        var match = href.match(/^https?:\/\/cl.ly\/[A-Za-z0-9]+\/?$/);
        if (!match) return;
        window.propane.requestJSON(message.id(), href, 'window.chat.cloudappexpander', 'onEmbedDataLoaded', 'onEmbedDataFailed');
      }
    },

    onEmbedDataLoaded: function(messageID, data) {
      var message = window.chat.transcript.getMessageById(messageID);
      if (!message) return;

      if (data['item_type'] === 'image') {
        var imageURL = data['content_url'];
        message.resize((function() {
          message.bodyCell.insert({bottom: '<div style="width:100%; margin-top:5px; padding-top: 5px; border-top:1px dotted #ccc;"><a href="'+imageURL+'" class="image loading" target="_blank">' + '<img src="'+imageURL+'" onload="$dispatch(&quot;inlineImageLoaded&quot;, this)" onerror="$dispatch(&quot;inlineImageLoadFailed&quot;, this)" /></a></div>'});
        }).bind(this));
      }
    },

    onEmbedDataFailed: function(messageID) {
      /* No cleanup required, we only alter the HTML after we get back a succesful load from the data */
    },

    onMessagesInsertedBeforeDisplay: function(messages) {
      for (var i = 0; i < messages.length; i++) {
        this.detectCloudAppURL(messages[i]);
      }
    },

    onMessageAccepted: function(message, messageID) {
      this.detectCloudAppURL(message);
    }
  });

  Campfire.Responders.push("CloudAppExpander");
  window.chat.installPropaneResponder("CloudAppExpander", "cloudappexpander");
}
