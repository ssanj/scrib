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
      const result = safely(function(){
        store.setItem(storage.storageKey, JSON.stringify(storage.data));
      });

      cb({
        "responseKey": responseKey,
        "data": result
      });
    } else if (storageAction === "delete") {
      const result = safely(function(){
        store.removeItem(storage.storageKey);
      })

      cb({
        "responseKey": responseKey,
        "data": result
      });
    } else if (storageAction === "load") {
      store.getItem(storage.storageKey); //fix
      console.log("storage loaded");
    } else if (storageAction === "clear") {
      store.clear();
      console.log("storage cleared");
    } else {
      console.log("unknown storage action: " + JSON.stringify(action.storage));
    }

  }
}

function safely(f) {
  try {
    f();
    return true;
  }
  catch(err) {
    console.log("safely error: " + err);
    return false; //it might be nice to return an error
  }
}

function findStorageType(storageType) {
  if (storageType === "session") {
    return sessionStorage;
  } else {
    return localStorage
  }
}

