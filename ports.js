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
  } else {
    console.log("error, handled action:" + JSON.stringify(action));
  }
}