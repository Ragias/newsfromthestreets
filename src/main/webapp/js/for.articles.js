function forIndex() {
	var greece = {
		'lat' : 38.01861070670554,
		'lng' : 23.625567382812505
	}
	var mapOptions = {
		center : new google.maps.LatLng(greece.lat, greece.lng),
		zoom : 8,
		mapTypeId : google.maps.MapTypeId.ROADMAP
	};

	var map = new google.maps.Map(document.getElementById("map_canvas"), mapOptions);
	var placeLat = 'All';
	var placeLng = 'All';
	var input = document.getElementById('searchTextField');
	var autocomplete = new google.maps.places.Autocomplete(input);

	autocomplete.bindTo('bounds', map);

	var infowindow = new google.maps.InfoWindow();
	var marker1 = new google.maps.Marker({
		map : map
	});

	$(document).ready(function() {
		var oldDateStr = "";
		var oldCategoryStr = "";
		var markerArr = new Array();

		var dateStr = "";
		var categoryStr = "";

		function isChanged() {
			if (oldDateStr == dateStr && oldCategoryStr == categoryStr) {
				return false;
			} else {
				return true;
			}
		}

		(function() {
			if (isChanged()) {
				google.maps.event.addListener(autocomplete, 'place_changed', function() {
					infowindow.close();
					var place = autocomplete.getPlace();
					if (place.geometry.viewport) {
						map.fitBounds(place.geometry.viewport);
						map.setZoom(8);
					} else {
						map.setCenter(place.geometry.location);
						map.setZoom(8);
						// Why 17? Because it looks good.
					}

					placeLat = place.geometry.location.lat();
					placeLng = place.geometry.location.lng();
					var address = '';
					if (place.address_components) {
						address = [(place.address_components[0] && place.address_components[0].short_name || ''), (place.address_components[1] && place.address_components[1].short_name || ''), (place.address_components[2] && place.address_components[2].short_name || '')].join(' ');
					}

					//infowindow.setContent('<div><strong>' + place.name + '</strong><br>' + address);
					infowindow.open(map, marker1);
					if (!markerArr.isEmpty) {
						while (markerArr[0]) {
							markerArr.pop().setMap(null);
						}
					}
				});
				console.log(oldDateStr + " == " + dateStr + " " + oldCategoryStr + " == " + categoryStr)

				if (!markerArr.isEmpty) {
					while (markerArr[0]) {
						markerArr.pop().setMap(null);
					}
				}
				oldDateStr = dateStr
				oldCategoryStr = categoryStr
			}

			dateStr = $("#the_date option:selected").text();
			categoryStr = $("#the_category option:selected").text();
			if (dateStr == "") {
				dateStr = "All";
			}
			if (categoryStr == "") {
				categoryStr = "All";
			}
			console.log("/api/newsfromthestreets/articles/" + categoryStr + "/" + dateStr + "?lat=" + placeLat + "&lng=" + placeLng);
			$.getJSON("/api/newsfromthestreets/articles/" + categoryStr + "/" + dateStr + "?lat=" + placeLat + "&lng=" + placeLng, function(json) {
				createNewList(json);

				if (json.length > 0) {
					for ( i = 0; i < json.length; i++) {

						addMarker(json[i]);
					}
				}
			});
			setTimeout(arguments.callee, 1000);
		})();
		function createNewList(json) {
			$('#listOfArticles').empty();
			for (var i = 0; i <= json.length - 1; i++) {
				$(' #listOfArticles').append($('<li>').append($('<a>').attr('href', '/article?q=show&id=' + json[i].id).append($('<span>').attr('class', 'tab').append(json[i].title))));
			}
		};
		function addMarker(location) {

			var check = false;
			for ( i = 0; i < markerArr.length; i++) {
				//console.log(markerArr[i].id + " " + location.id + " " + markerArr.length)
				if (markerArr[i].id == location.id) {
					check = true;

					if (location.latlng.lat == markerArr[i].position.lat() && location.latlng.long == markerArr[i].position.lng()) {
						break;
					}
					var point = new google.maps.LatLng(location.latlng.lat, location.latlng.long);

					var marker = new google.maps.Marker({
						position : point,
						map : map,
						title : location.title,
						id : location.id
					});
					markerArr[i].setMap(null)
					markerArr[i] = marker;
					break;
				}
			}
			if (!check) {
				console.log(location.title)
				var point = new google.maps.LatLng(location.latlng.lat, location.latlng.lng);

				var marker = new google.maps.Marker({
					position : point,
					map : map,
					title : location.title,
					id : location.id
				});
				markerArr.push(marker);
			}

		};
	});

};





function forArticle() {

	$.getJSON("/api/newsfromthestreets/article/" + getParameterByName("id"), function(json) {
		console.log(json.latlng.lat + " " + json.latlng.long + " " + json.title);
		var lat = json.latlng.lat;
		var lng = json.latlng.long;

		var mapOptions = {
			zoom : 9,
			center : new google.maps.LatLng(lat, lng),
			mapTypeId : google.maps.MapTypeId.ROADMAP
		};

		var map = new google.maps.Map(document.getElementById('map_canvas'), mapOptions);

		var point = new google.maps.LatLng(lat, lng);

		var marker = new google.maps.Marker({
			position : point,
			map : map,
			title : location.title,
			id : location.id
		});

		function placeMarker(location) {
			if (marker) {
				marker.setPosition(location);
			} else {
				marker = new google.maps.Marker({
					position : location,
					map : map

				});
			}
		}


		google.maps.event.addListener(map, 'click', function(event) {
			document.getElementById('the_lat').value = event.latLng.lat();
			document.getElementById('the_lng').value = event.latLng.lng();
			//alert(event.latLng.lat()+" "+event.latLng.lng())
			placeMarker(event.latLng);
		});
	});

	google.maps.event.addDomListener(window, 'load', initialize);
	showListOfComments(getParameterByName("id"),getParameterByName("num"));
};

function forAddArticle() {
	var latStr = "";
	var lngStr = "";
	$(document).ready(function() {
		latStr = $(".lat").text();
		lngStr = $(".lng").text();
	});
	var latFloat = parseFloat(latStr);
	var lngFloat = parseFloat(lngStr);
	if (!isNaN(latFloat) && !isNaN(lngFloat)) {
		var mapOptions = {
			zoom : 4,
			center : new google.maps.LatLng(latFloat, lngFloat),
			mapTypeId : google.maps.MapTypeId.ROADMAP
		};

		var map = new google.maps.Map(document.getElementById('map_canvas'), mapOptions);

		var marker = new google.maps.Marker({
			position : map.getCenter(),
			map : map,
			title : 'Click to zoom'
		});

		google.maps.event.addListener(map, 'center_changed', function() {
			// 3 seconds after the center of the map has changed, pan back to the
			// marker.
			window.setTimeout(function() {
				map.panTo(marker.getPosition());
			}, 3000);
		});

		google.maps.event.addListener(marker, 'click', function() {
			map.setZoom(8);
			map.setCenter(marker.getPosition());
		});
	} else {
		var marker;

		var greece = {
			'lat' : 38.01861070670554,
			'lng' : 23.625567382812505
		}

		var mapOptions = {
			center : new google.maps.LatLng(greece.lat, greece.lng),
			zoom : 5,
			mapTypeId : google.maps.MapTypeId.ROADMAP
		};

		var map = new google.maps.Map(document.getElementById('map_canvas'), mapOptions);
		function placeMarker(location) {
			if (marker) {
				marker.setPosition(location);
			} else {
				marker = new google.maps.Marker({
					position : location,
					map : map

				});
			}
		}


		google.maps.event.addListener(map, 'click', function(event) {
			document.getElementById('the_lat').value = event.latLng.lat();
			document.getElementById('the_lng').value = event.latLng.lng();
			//alert(event.latLng.lat()+" "+event.latLng.lng())
			placeMarker(event.latLng);
		});

	}

	google.maps.event.addDomListener(window, 'load', initialize);
};
