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

      backgrounded(cb, result);

    } else if (storageAction === "delete_key") {

      const action = function(){
        store.removeItem(storage.storageKey);
      };

      const resultTransformer = function(result) {
        return null;
      }

      const result = execute(action, responseKey, resultTransformer);

      backgrounded(cb, result);
    } else if (storageAction === "delete_from_array") {
      console.log("delete_from_array called with: " + JSON.stringify(storage))
    //   const action = function() {
    //     const cacheArrayObj = getJSONArrayKey(store, storage.storageKey);
    //     //remove key from array
    //     //TODO: Find out the correct structure for this
    //     const index = cacheArrayObj.findIndex((el) => el.noteId == storage.data.noteId)
    //     if (index != -1) {
    //       cacheArrayObj.splice(index, 1);
    //       const updatedCacheString =   JSON.stringify(cacheArrayObj);
    //       store.setItem(storage.storageKey, updatedCacheString);
    //     } else { }
    //   };

    //   const resultTransformer = function(result) {
    //     return result;
    //   }

    //   const result = execute(action, responseKey, resultTransformer);

    //   backgrounded(cb, result);
    } else if (storageAction === "load") {
      const action = function() {
        store.getItem(storage.storageKey);
      };

      const resultTransformer = function(result) {
        return result;
      }

      const result = execute(action, responseKey, resultTransformer);

      backgrounded(cb, result);
    } else if (storageAction === "add_to_array") {
      const action = function() {
        const cacheArrayObj = getJSONArrayKey(store, storage.storageKey);
        cacheArrayObj.unshift(storage.data);
        const updatedCacheString = JSON.stringify(cacheArrayObj);
        store.setItem(storage.storageKey, updatedCacheString);
      };

      const resultTransformer = function(result) {
        return result;
      }

      const result = execute(action, responseKey, resultTransformer);

      backgrounded(cb, result);
    } else if (storageAction === "update_to_array") {
      const action = function() {
        const cacheArrayObj = getJSONArrayKey(store, storage.storageKey);

        const foundIndex = cacheArrayObj.findIndex(o => o.noteId === storage.data.noteId)

        if (foundIndex != -1) {
          cacheArrayObj[foundIndex] = storage.data
          const updatedCacheString = JSON.stringify(cacheArrayObj);
          store.setItem(storage.storageKey, updatedCacheString);
        } else {
         console.log("update_to_array: Did not find matching item in for: " + JSON.stringify(storage.data));
        }
      };

      const resultTransformer = function(result) {
        return result;
      }

      const result = execute(action, responseKey, resultTransformer);

      backgrounded(cb, result);
    } else if (storageAction === "clear") {

      const action = function() {
        store.clear();
      }

      const resultTransformer = function(result) {
        return null;
      }

      const result = execute(action, responseKey, resultTransformer);

      backgrounded(cb, result);
    } else {
      const result = errorPayload(responseKey, "unknown storage action: " + JSON.stringify(action.storage));

      backgrounded(cb, result);
    }

  }
}

//try this to see if it fixes the lost callback issue.
function backgrounded(callback, result) {
  setTimeout(callback(result), 0);
}

function getJSONArrayKey(store, key) {
  const maybeCacheString = store.getItem(key);
  return (maybeCacheString === null) ? [] : JSON.parse(maybeCacheString)
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

