function initialize() {
    var mapCenter = new google.maps.LatLng(51.50853, -0.12574);
    var map = new google.maps.Map(document.getElementById('map_canvas'), {
        zoom: 4,
        center: mapCenter, mapTypeId:
        google.maps.MapTypeId.HYBRID
    });

    var marker = new google.maps.Marker({
        map: map,
        position: mapCenter,
        title: 'London'
    });

    var circle = new google.maps.Circle({
        map: map,
        radius: 804672, // 500 miles in meter
        fillColor: '#AA0000'
    });

    circle.bindTo('center', marker, 'position');
}

google.maps.event.addDomListener(window, 'load', initialize);
