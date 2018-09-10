TYPE=$1
LEVEL=$2

cat >>${TYPE}_${LEVEL}mb.html<<-EOF
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
		<title>NOAA EMC Conventional Station Data Monitoring</title>
		<link rel="stylesheet" href="css/style.css" type="text/css" media="all" />
		<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"></script>
            <script src="${TYPE}_${LEVEL}mb.js" type="text/javascript"></script>

	</head>

    <body>

    <div id="map" style="width: 950px; height: 750px"></div>

    <table>
    <tr>
    <td><img src="./markers/blue-dot.png" /></td><td>Normal &nbsp; &nbsp;</td>
    <td><img src="./markers/orange-dot.png" /></td><td>Bias &nbsp; &nbsp;</td>
    <td><img src="./markers/red-dot.png" /></td><td>Reject&nbsp; &nbsp;</td>
    </tr>
    </table>

    <a href="http://www.emc.ncep.noaa.gov/">NOAA EMC Homepage</a>

    <script type="text/javascript">

  var infowindow = null;

  var blueIcon = new google.maps.MarkerImage('./markers/blue-dot.png',
      // This marker is 20 pixels wide by 32 pixels tall.
      new google.maps.Size(32, 32),
      // The origin for this image is 0,0.
      new google.maps.Point(0,0),
      // The anchor for this image is the base of the flagpole at 0,32.
      new google.maps.Point(0, 0));

  var orangeIcon = new google.maps.MarkerImage('./markers/orange-dot.png',
      // This marker is 20 pixels wide by 32 pixels tall.
      new google.maps.Size(32, 32),
      // The origin for this image is 0,0.
      new google.maps.Point(0,0),
      // The anchor for this image is the base of the flagpole at 0,32.
      new google.maps.Point(0, 0));

  var redIcon = new google.maps.MarkerImage('./markers/red-dot.png',
      // This marker is 20 pixels wide by 32 pixels tall.
      new google.maps.Size(32, 32),
      // The origin for this image is 0,0.
      new google.maps.Point(0,0),
      // The anchor for this image is the base of the flagpole at 0,32.
      new google.maps.Point(0, 0));

  var yellowIcon = new google.maps.MarkerImage('./markers/yellow-dot.png',
      // This marker is 20 pixels wide by 32 pixels tall.
      new google.maps.Size(32, 32),
      // The origin for this image is 0,0.
      new google.maps.Point(0,0),
      // The anchor for this image is the base of the flagpole at 0,32.
      new google.maps.Point(0, 0));

  var shadow = new google.maps.MarkerImage('./markers/msmarker.shadow.png',
      // The shadow image is larger in the horizontal dimension
      // while the position and offset are the same as for the main image.
      new google.maps.Size(56, 32),
      new google.maps.Point(0,0),
      new google.maps.Point(0, 0));

      // A function to create the marker and set up the event window
      function createMarkers(map, locations) {
         var myIcon;
         for (var i = 0; i < locations.length; i++) {
            var site = locations[i];
            var myLatLng = new google.maps.LatLng(site[0], site[1]);

            if(site[2]=="norm")
               myIcon=blueIcon
            else if(site[2]=="bias")
               myIcon=orangeIcon
            else if(site[2]=="reje")
               myIcon=redIcon
//            else if(site[2]=="qrej")
//               myIcon=yellowIcon
            else 
               myIcon=blueIcon

            var marker = new google.maps.Marker({
                position: myLatLng,
                map: map,
                icon: myIcon,
                shadow: shadow,
                html: site[3]

            });

            google.maps.event.addListener(marker, "click", function () {

               // Check to see if an InfoWindow already exists
               if (!infowindow) {
                   infowindow = new google.maps.InfoWindow();
               }

               <!--alert(this.html);-->
               infowindow.setContent(this.html);
               infowindow.open(map, this);
            });
        }

      }



 window.onload = function() {

      // Creating a map centered wwb
      var options = {  
        zoom: 2,
        center: new google.maps.LatLng(38.823040, -76.917880),  
        mapTypeId: google.maps.MapTypeId.ROADMAP  
      };  
    
     var map = new google.maps.Map(document.getElementById('map'), options);
     
     createMarkers(map, sites);
//		infowindow = new google.maps.InfoWindow({
//                content: "loading..."
//            });
 }

    </script>
  </body>

</html>

EOF
