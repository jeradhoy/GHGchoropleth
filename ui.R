#global.R script for choropleth

library(shiny)
library(leaflet)
library(rCharts)
library(ShinyDash)
library(shinyBS)

#options(RCHART_LIB = 'NVD3')

shinybootstrap2::withBootstrap2({

shinyUI(fluidPage(title="Poulter Lab | State Emissions Reduction Explorer",

# HTML header tags for facebook and twitter sharing
	tags$head(HTML('
<meta property="og:url" content="http://bit.ly/1vtDKlh" />
<meta property="og:title" content="Poulter Lab Emissions Reduction Explorer" />
<meta property="og:description" content="Learn how you can reduce state CO2 emissions to reach Obama&#39;s emissions goal!" />
<!-- <meta property="og:image" content="http://153.90.240.137:3838/choropleth/ChoroplethScreenshot1.png" /> -->'
	)),

	tags$head(
	# Include our custom CSS
	includeCSS("styles.css"),
	HTML("

<div id='fb-root'></div>
<script>(function(d, s, id) {
    var js, fjs = d.getElementsByTagName(s)[0];
    if (d.getElementById(id)) return;
    js = d.createElement(s); js.id = id;
    js.src = '//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.0';
    fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));
</script>


<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
<script>
 /*svg { -webkit-touch-callout: none; -webkit-user-select: none; -khtml-user-select: none; -moz-user-select: none; -ms-user-select: none; user-select: none; display: block; width:100%; height:100%; } svg text { font: normal 12px Arial; } svg .title { font: bold 14px Arial; } */
 </script>

)"

	),


# Add a little CSS to make the map background pure white
	tags$style(type="text/css", "
#showcase-code-position-toggle, #showcase-sxs-code { display: none; }

.floater { background-color: white; padding: 8px; opacity: .9; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2);}

#.floater1 { background-color: NULL; padding: 8px; opacity: .9; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
.clearBox { background-color: NULL; opacity: .1;}
.infoBox { background-color: white; padding: 8px; opacity: 1; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
.socialBox { background-color: clear; padding: 0px; opacity: 1; border-radius: 0px;}
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before { visibility: hidden; }
.leaflet-top, .leaflet-bottom {z-index: -10;}
.btn {width: 69px; padding: 4px 1px;}
.legendText {
    font-size:50px;
    font-family:'Arial, sans-serif';
    font-weight: bold;
}
h4 {
    margin: 0px;
    margin-bottom: none;
    margin-top: 8px;
}"
	),


	div(class="outer",



	# Initializing the leaflet map
	leafletMap("map", width="100%", height="100%",
		# Set tile provider, thunderforest landscape used here
		initialTileLayer = "//{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png",
		initialTileLayerAttribution = NULL,

		# Set initial position, zoom level, and bounds of the leaflet map
	   options=list(
		 center = c(40, -94.85),
		 zoom = 4,
		 minZoom = 4,
		 maxZoom = 4,
		 maxBounds = list(list(17, -160), list(59, -45))
	   )

	 ),

	# Twitter and facebook panel in bottom left corner
	absolutePanel(
		left = "1%", bottom = "1%", class = "socialBox", fixed = TRUE, draggable = F,
		HTML("
			<div style='white-space: pre; opacity: 1;'>
			<a class='twitter-share-button'
			href='http://bit.ly/1vtDKlh'
			data-url='http://bit.ly/1vtDKlh'
			data-size='large'
			data-count='none'
			data-counturl='http://bit.ly/1vtDKlh'>Tweet</a>
			<div class='fb-like'
			data-href='http://bit.ly/1vtDKlh'
			data-width='90'
			data-layout='button_count'
			data-action='like'
			ata-show-faces='true'
			data-share='true'>
			</div></div>"
		)
	),

	# MSU and IOE panel in bottom right coner
	absolutePanel(
		right = "1.1%", bottom = ".5%", class = "socialBox", fixed = TRUE, draggable = F,
			HTML("<div style='text-align: right'>
				<a href='http://montana.edu'><img src='MSU-horiz.png' style='height:30px; vertical-align: bottom; opacity: .7;'></a>
				&nbsp;&nbsp;<a href='http://montanaioe.org'><img src='mus-ioe-logo-horizontal-1200w.png' style='height:28px; vertical-align: bottom;opacity: .7;'></a><br><span style='font-size: 9pt'><a href='http://leafletjs.com'>Leaflet</a> | © <a href='www.openstreetmap.org/copyright'>OpenStreetMap</a> Contributors & <a href='www.thunderforest.com'>Thunderforest</a></span></br></div>"
		)
	),

	# Left side panel with target text, gauges and butoons
    absolutePanel(
        left = "1%", top = "1%", width = 215, class = "floater", fixed = TRUE, draggable = F,

		h4(tags$strong(HTML("<div style='text-align:center;'>State and National Emissions Targets</div>"))),
		uiOutput("stateGaugeOutput"),
		uiOutput("stateText2"),
		uiOutput("USGaugeOutput"),
		uiOutput("USText2"),
		tags$div(style="text-align:center; margin-top: 8px;",
		HTML("<b>Choose how to color the map</b>"),
		bsButtonGroup("btngrp1", toggle = "radio", value = "perCapita",
		bsButton("btn1", label = "Per Capita", value = "perCapita"),
		bsButton("btn2", label = "Total", value = "total"))),
		tags$div(class='row-fluid', style='margin-top: 15px',
		actionButton("reset", label = "Reset"),
		actionButton("info", label = "Info"),
		actionButton("exercise", label = "Exercises"))

    ),




	# Right side panel with sliders and pie chart
	absolutePanel(
		right = "1%", top = "1%", width = 205, class = "floater", fixed = TRUE, draggable = F,

		uiOutput("sliderTitle"),
		HTML("<span id='commSliderTitle' style='color:darkblue; cursor:pointer; font-weight: 400;'>Commercial CO<sub>2</sub> Emissions</span>"),

		uiOutput("commSlider"),
		HTML("<span id='industSliderTitle' style='color: darkorange; cursor:pointer; font-weight: 400;'>Industrial CO<sub>2</sub> Emissions</span>"),

		uiOutput("industSlider"),
		HTML("<span id='residSliderTitle' style='color:darkgreen; cursor:pointer; font-weight: 400;'>Residential CO<sub>2</sub> Emissions</span>"),

		uiOutput("residSlider"),
		HTML("<span id='transSliderTitle' style='color:red; cursor:pointer; font-weight: 400;'>Transportation CO<sub>2</sub> Emissions</span>"),

		uiOutput("transSlider"),
		HTML("<span id='elecSliderTitle' style='color:purple; cursor:pointer; font-weight: 400;'>Electricity CO<sub>2</sub> Emissions</span>"),

		uiOutput("elecSlider"),
		uiOutput("pieTitle"),
		showOutput("rChart", "nvd3")
    ),

	uiOutput("helpWindow"),
	uiOutput("exerciseWindow")


)
)
)
)

})
