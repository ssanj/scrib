function portActions(action, cb) {
  // if (action.eventType === "save_message") {
  //   saveMessageToLocalStorage(noteEvent);
  // } else if (noteEvent.eventType === "save_view_session") {
  //   saveToSessionStorage(noteEvent);
  // } else if (noteEvent.eventType=== "preview_message") {
  //   previewMarkdown(noteEvent)
  // } else if (noteEvent.eventType=== "remove_message") {
  //   console.log("clearing local storage");
  //   localStorage.removeItem(appEditKey);
  //   app.ports.jsMessage.send(responseJson("message_removed"));
  // } else if (noteEvent.eventType=== "log_message_to_console") {
  if (action.eventType === "log_action") {
    console.log("[" + JSON.stringify(action.log.appName) + "]: " + JSON.stringify(action.log.data));
  } else if (action.eventType === "storage_action") {
    handleStorage(action, cb);
  } else if (action.eventType === "markdown_action") {
    handleMarkdown(action);
  } else {
    console.log("error, handled action:" + JSON.stringify(action));
  }
}

function handleMarkdown(action) {
  const markdown       = action.markdown;
  const elementId       = markdown.elementId;
  const markdownContent = markdown.data.noteText;

  const markedOptions = {
    "gfm": true,
    "smartLists": true,
    "xhtml": true
  }

  const markdownView = document.getElementById(elementId);
    if (typeof marked !== 'undefined' && markdownView) {
      markdownView.innerHTML = marked(markdownContent, markedOptions);
    } else {
     console.log("markdown renderer has not loaded.");
    }
}

function handleStorage(action, cb) {
  const storage       = action.storage;
  const store         = findStorageType(storage.storageType)
  const storageAction = storage.storageAction;
  const responseKey   = action.responseKey;

  if (!responseKey) {
    console.log("Did not find responseKey in action: " + JSON.stringify(action)); //if we had appKey we could call log.
  } else {

    if (storageAction === "save") {
      const action = function(){
        store.setItem(storage.storageKey, JSON.stringify(storage.data));
      };

      const resultTransformer = function(result) {
        return null;
      }

      const result = execute(action, responseKey, resultTransformer);

      cb(result);

    } else if (storageAction === "delete") {

      const action = function(){
        store.removeItem(storage.storageKey);
      };

      const resultTransformer = function(result) {
        return null;
      }

      const result = execute(action, responseKey, resultTransformer);

      cb(result);
    } else if (storageAction === "load") {
      const action = function() {
        store.getItem(storage.storageKey);
      };

      const resultTransformer = function(result) {
        return result;
      }

      const result = execute(action, responseKey, resultTransformer);

      cb(result);
    } else if (storageAction === "clear") {

      const action = function() {
        store.clear();
      }

      const resultTransformer = function(result) {
        return null;
      }

      const result = execute(action, responseKey, resultTransformer);

      cb(result);
    } else {
      const result = errorPayload(responseKey, "unknown storage action: " + JSON.stringify(action.storage));

      cb(result);
    }

  }
}

function execute(f, responseKey, dataTransform) {
  try {
    return {
      "responseKey": responseKey,
      "data": dataTransform(f())
    };
  }
  catch(err) {
    return errorPayload(responseKey, err.toString());
  }
}

function errorPayload(responseKey, errorString) {
    return {
      "responseKey": responseKey,
      "error": errorString
    };
}

function findStorageType(storageType) {
  if (storageType === "session") {
    return sessionStorage;
  } else {
    return localStorage
  }
}

