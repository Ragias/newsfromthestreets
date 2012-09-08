function handleNoGeolocation(errorFlag) {
	if (errorFlag) {
		var content = 'Error: The Geolocation service failed.';
	} else {
		var content = 'Error: Your browser doesn\'t support geolocation.';
	}
	$.ajax({
		url : "/api/detectivesetmode",
		type : "PUT",
		dataType : 'json',
		contentType : 'application/json',
		data : JSON.stringify({
			"mode" : false
		})
	});
	var groupId = getParameterByName("id")
	if (groupId) {
		$.ajax({
			url : "/api/ingroup/setmode",
			type : "PUT",
			dataType : 'json',
			contentType : 'application/json',
			data : JSON.stringify({
				"groupId" : groupId,
				"mode" : false
			})
		});
	}
	var options = {
		map : map,
		position : new google.maps.LatLng(60, 105),
		content : content
	};

	var infowindow = new google.maps.InfoWindow(options);
	map.setCenter(options.position);
};

function forDetectives(id) {
	var map;
	var markerArr = new Array();

	var watch;
	var mapOptions = {
		zoom : 6,
		mapTypeId : google.maps.MapTypeId.ROADMAP
	};
	map = new google.maps.Map(document.getElementById('map_canvas'), mapOptions);
	// Try HTML5 geolocation
	if (navigator.geolocation) {

		navigator.geolocation.watchPosition(function(position) {
			var pos = new google.maps.LatLng(position.coords.latitude, position.coords.longitude);
			map.setCenter(pos);

			document.getElementById('currentLat').innerHTML = position.coords.latitude;
			document.getElementById('currentLon').innerHTML = position.coords.longitude;

			$.ajax({
				url : "/api/detective",
				type : "PUT",
				dataType : 'json',
				contentType : 'application/json',
				data : JSON.stringify({
					"latlng" : {
						"lat" : position.coords.latitude,
						"long" : position.coords.longitude
					}
				})
			});

			if (id != "None") {
				$.ajax({
					url : "/api/ingroup/setmode",
					type : "PUT",
					dataType : 'json',
					contentType : 'application/json',
					data : JSON.stringify({
						"groupId" : id,
						"mode" : true
					})
				});
			}

			var checkJson = (function() {

				$.getJSON("/api/searchgroup/" + id, function(json) {

					if (json.length > 0) {
						for ( i = 0; i < json.length; i++) {
							var location = json[i].latlng;
							addMarker(location);
						}
					}
					if (json.length < markerArr.length) {
						if (!markerArr.isEmpty) {
							while (markerArr[0]) {
								markerArr.pop().setMap(null);
							}
						}
					}

				});

				setTimeout(arguments.callee, 1000);

			});
			checkJson();
		}, function() {
			handleNoGeolocation(true);
		}, {
			enableHighAccuracy : true,
			maximumAge : 100000,
			timeout : 100000
		});

	} else {

		// Browser doesn't support Geolocation
		handleNoGeolocation(false);
	}

	function addMarker(location) {

		var check = false;
		for ( i = 0; i < markerArr.length; i++) {
			
			if (markerArr[i].id == location.id) {
				check = true;

				if (location.lat == markerArr[i].position.lat() && location.long == markerArr[i].position.lng()) {
					break;
				}
				var point = new google.maps.LatLng(location.lat, location.long);

				var marker = new google.maps.Marker({
					position : point,
					map : map,
					title : location.username,
					id : location.id
				});
				markerArr[i].setMap(null)
				markerArr[i] = marker;
				break;
			}
		}
		if (!check) {
			//console.log(location.title)
			var point = new google.maps.LatLng(location.lat, location.long);

			var marker = new google.maps.Marker({
				position : point,
				map : map,
				title : location.username,
				id : location.id
			});
			markerArr.push(marker);
		}

	};

	google.maps.event.addDomListener(window, 'load', initialize);
};

function forListOfSearchGroups() {
	var greece = {
		'lat' : 38.01861070670554,
		'lng' : 23.625567382812505
	}
	var map;
	var markerArr = new Array();
	var watch;
	var mapOptions = {
		center : new google.maps.LatLng(greece.lat, greece.lng),
		zoom : 6,
		mapTypeId : google.maps.MapTypeId.ROADMAP
	};
	map = new google.maps.Map(document.getElementById('map_canvas'), mapOptions);
	// Try HTML5 geolocation

	var checkJson = (function() {
		var searchGroupStr = $("#the_searchGroup option:selected").attr('value');

		$.getJSON("/api/searchgroup/None", function(json) {

			if (json.length > 0) {
				for ( i = 0; i < json.length; i++) {
					var location = json[i].latlng;
					addMarker(location);
				}
			}
			if (json.length < markerArr.length) {
				if (!markerArr.isEmpty) {
					while (markerArr[0]) {
						markerArr.pop().setMap(null);
					}
				}
			}

		});

		setTimeout(arguments.callee, 1000);

	});
	checkJson();

	function addMarker(location) {

		var check = false;
		for ( i = 0; i < markerArr.length; i++) {
			console.log(markerArr[i].id + " " + location.id + " " + markerArr.length + " lat " + location.lat + " long " + location.long)
			if (markerArr[i].id == location.id) {
				check = true;

				if (location.lat == markerArr[i].position.lat() && location.long == markerArr[i].position.lng()) {
					break;
				}
				var point = new google.maps.LatLng(location.lat, location.long);

				var marker = new google.maps.Marker({
					position : point,
					map : map,
					id : location.id,
					title : location.username
				});
				markerArr[i].setMap(null)
				markerArr[i] = marker;
				break;
			}
		}
		if (!check) {
			//console.log(location.title)
			var point = new google.maps.LatLng(location.lat, location.long);

			var marker = new google.maps.Marker({
				position : point,
				map : map,
				title : location.username,
				id : location.id
			});
			markerArr.push(marker);
		}

	};

	google.maps.event.addDomListener(window, 'load', initialize);
};

function forSearchGroup() {
	var map;
	var markerArr = new Array();
	var greece = {
		'lat' : 38.01861070670554,
		'lng' : 23.625567382812505
	}

	var watch;
	var mapOptions = {
		center : new google.maps.LatLng(greece.lat, greece.lng),
		zoom : 6,
		mapTypeId : google.maps.MapTypeId.ROADMAP
	};
	map = new google.maps.Map(document.getElementById('map_canvas'), mapOptions);
	// Try HTML5 geolocation

	var checkJson = (function() {
		var searchGroupStr = getParameterByName("id");

		$.getJSON("/api/searchgroup/" + searchGroupStr, function(json) {

			if (json.length > 0) {
				for ( i = 0; i < json.length; i++) {
					var location = json[i];
					addMarker(location);
				}
			}
			if (json.length < markerArr.length) {
				if (!markerArr.isEmpty) {
					while (markerArr[0]) {
						markerArr.pop().setMap(null);
					}
				}
			}

		});

		setTimeout(arguments.callee, 1000);

	});
	checkJson();

	function addMarker(location) {

		var check = false;
		for ( i = 0; i < markerArr.length; i++) {

			if (markerArr[i].title == location.user_id) {
				check = true;

				if (location.latlng.lat == markerArr[i].position.lat() && location.latlng.long == markerArr[i].position.lng()) {
					break;
				}
				var point = new google.maps.LatLng(location.latlng.lat, location.latlng.long);

				var marker = new google.maps.Marker({
					position : point,
					map : map,
					title : location.user_id
				});
				markerArr[i].setMap(null)
				markerArr[i] = marker;
				break;
			}
		}
		if (!check) {
			//console.log(location.title)
			var point = new google.maps.LatLng(location.latlng.lat, location.latlng.long);

			var marker = new google.maps.Marker({
				position : point,
				map : map,
				title : location.user_id
			});
			markerArr.push(marker);
		}

	};
	google.maps.event.addDomListener(window, 'load', initialize);

};

function forMobileSearchGroup() {
	var greece = {
		'lat' : 38.01861070670554,
		'lng' : 23.625567382812505
	}
	var map;
	var markerArr = new Array();
	var watch;
	var mapOptions = {
		center : new google.maps.LatLng(greece.lat, greece.lng),
		zoom : 6,
		mapTypeId : google.maps.MapTypeId.ROADMAP
	};
	map = new google.maps.Map(document.getElementById('map_canvas'), mapOptions);
	// Try HTML5 geolocation

	var checkJson = (function() {
		var searchGroupStr = getParameterByName("id");
		if (searchGroupStr) {
			$.getJSON("/api/searchgroup/" + searchGroupStr, function(json) {

				if (json.length > 0) {
					for ( i = 0; i < json.length; i++) {
						var location = json[i];
						addMarker(location);
					}
				}
				if (json.length < markerArr.length) {
					if (!markerArr.isEmpty) {
						while (markerArr[0]) {
							markerArr.pop().setMap(null);
						}
					}
				}

			});

			setTimeout(arguments.callee, 1000);
		}
	});
	checkJson();

	function addMarker(location) {

		var check = false;
		for ( i = 0; i < markerArr.length; i++) {

			if (markerArr[i].id == location.id) {
				check = true;

				if (location.latlng.lat == markerArr[i].position.lat() && location.latlng.long == markerArr[i].position.lng()) {
					break;
				}
				var point = new google.maps.LatLng(location.latlng.lat, location.latlng.long);

				var marker = new google.maps.Marker({
					position : point,
					map : map,
					title : location.username,
					id : location.id
				});
				markerArr[i].setMap(null)
				markerArr[i] = marker;
				break;
			}
		}
		if (!check) {
			//console.log(location.title)
			var point = new google.maps.LatLng(location.latlng.lat, location.latlng.long);

			var marker = new google.maps.Marker({
				position : point,
				map : map,
				title : location.username,
				id : location.id
			});
			markerArr.push(marker);
		}

	};
	google.maps.event.addDomListener(window, 'load', initialize);
}
