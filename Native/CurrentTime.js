Elm.Native.CurrentTime = {};
Elm.Native.CurrentTime.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.CurrentTime = localRuntime.Native.CurrentTime || {};
	if (localRuntime.Native.CurrentTime.values)
	{
		return localRuntime.Native.CurrentTime.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function log(string)
	{
		return Task.asyncFunction(function(callback) {
			console.log(string);
			return callback(Task.succeed(Utils.Tuple0));
		});
	}


	var getCurrentTime = Task.asyncFunction(function(callback) {
		return callback(Task.succeed(Date.now()));
	});


	return localRuntime.Native.CurrentTime.values = {
		log: log,
		getCurrentTime: getCurrentTime
	};
};
