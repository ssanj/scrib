function portActions(action) {
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
    console.log("log: " + JSON.stringify(action.data));
  } else if (action.eventType === "storage_action") {
    handleStorage(action)
  } else {
    console.log("error, handled action:" + JSON.stringify(action));
  }
}

//we need a way to figure out how to respond to JS
function handleStorage(action) {
  const storage       = action.storage;
  const store         = findStorageType(storage.storageType)
  const storageAction = storage.storageAction;

  if (storageAction === "save") {
    store.setItem(storage.storageKey, JSON.stringify(storage.data));
    console.log("storage saved");
  } else if (storageAction === "delete") {
    store.removetem(storage.storageKey);
    console.log("storage deleted");
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

// function subscriptionAction(eventType, result) {
//   {
//     "eventType": eventType,
//     "data": JSON.parse(result);
//   }
// }

function findStorageType(storageType) {
  if (storageType === "session") {
    return sessionStorage;
  } else {
    return localStorage
  }
}

