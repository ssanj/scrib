function getApiKey(localStorage, appApiKey) {
  try {
    return JSON.parse(localStorage.getItem(appApiKey));
  }
  catch(err) {
    return null;
  }
}
