function portActions(action, cb) {
  if (action.eventType === "log_action") {
    console.log("[" + JSON.stringify(action.log.appName) + "]: " + JSON.stringify(action.log.data));
  } else if (action.eventType === "storage_action") {
    handleStorage(action, cb);
  } else {
    console.log("error, handled action:" + JSON.stringify(action));
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

