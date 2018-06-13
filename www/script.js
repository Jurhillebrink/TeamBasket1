$(document).keyup(function(event) {
    if ($("#uiUsername").is(":focus") && (event.keyCode == 13)) {
        $("#uiPassword").focus();
    }
    if ($("#uiPassword").is(":focus") && (event.keyCode == 13)) {
        $("#ok").click();
    }

    if ($("#eventPassword").is(":focus") && (event.keyCode == 13)) {
        $("#endEvent").click();
    }
});

$(document).on('shiny:visualchange', function(event) {
    $("#eventPassword").focus();
    $("#uiUsername").focus();
});

//Create new event position
$(document).ready(function() {

    $.getScript('mapster.js', function() {
        $('#fieldImage').mapster({
            fillOpacity: 0.75,
            fillColor: "#f92c2c",
            stroke: true,
            strokeColor: "red",
            strokeOpacity: 0.8,
            singleSelect: true,
            scaleMap: true,
            mapKey: 'name',
            listKey: 'name',
            onClick: function(e) {
                var keyValue = e.key;
                keyValue = keyValue.replace("location", "");
                // if Asparagus selected, change the tooltip
                keyValue = parseInt(keyValue);
                document.getElementById("sliderPosition").value = keyValue;
                $("#sliderPosition").val(keyValue);
                Shiny.onInputChange("sliderPosition", keyValue);

            },
            showToolTip: false,
            toolTipClose: ["tooltip-click", "area-click"],
            areas: [{
                    key: "location1",
                    fillColor: "f92c2c",
                    selected: true
                },
                {
                    key: "location2",
                    fillColor: "f92c2c"
                },
                {
                    key: "location3",
                    fillColor: "f92c2c"
                },
                {
                    key: "location4",
                    fillColor: "f92c2c"
                },
                {
                    key: "location5",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                }, {
                    key: "location6",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location7",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location8",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location9",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location10",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location11",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location12",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location13",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location14",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                }
            ]
        });

    })

    $("#switchtab").click(function() {
        setTimeout(function() {
            resize();
        }, 250);
    })

    $("#typeselector").click(function() {
        //alert($("#typeselector:checked").val());
        setTimeout(function() {
            if ($('input[name=typeselector]:checked').val() == "free_throw") {
                $('#fieldImage').css("opacity", 1);
                //$("option:selected").removeAttr("selected");
            } else {
                $('#fieldImage').css("opacity", 0.2);
                //$("option:selected").removeAttr("selected");
            }
        }, 250);
    })


});

///team3
//Create new event position
$(document).ready(function() {

    $.getScript('mapster.js', function() {
        $('#fieldImage31').mapster({
            fillOpacity: 0.75,
            fillColor: "#f92c2c",
            stroke: true,
            strokeColor: "red",
            strokeOpacity: 0.8,
            singleSelect: true,
            scaleMap: true,
            mapKey: 'name',
            listKey: 'name',
            onClick: function(e) {
                var keyValue = e.key;
                keyValue = keyValue.replace("location", "");
                // if Asparagus selected, change the tooltip
                keyValue = parseInt(keyValue);
                document.getElementById("sliderPosition31").value = keyValue;
                $("#sliderPosition").val(keyValue);
                Shiny.onInputChange("sliderPosition31", keyValue);

            },
            showToolTip: false,
            toolTipClose: ["tooltip-click", "area-click"],
            areas: [{
                    key: "location1",
                    fillColor: "f92c2c",
                    selected: true
                },
                {
                    key: "location2",
                    fillColor: "f92c2c"
                },
                {
                    key: "location3",
                    fillColor: "f92c2c"
                },
                {
                    key: "location4",
                    fillColor: "f92c2c"
                },
                {
                    key: "location5",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                }, {
                    key: "location6",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location7",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location8",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location9",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location10",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location11",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location12",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location13",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location14",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                }
            ]
        });

    })

    $("#switchtab31").click(function() {
        setTimeout(function() {
            resize();
        }, 250);
    })

    $("#typeselector31").click(function() {
        //alert($("#typeselector31:checked").val());
        setTimeout(function() {
            if ($('input[name=typeselector31]:checked').val() == "free_throw") {
                $('#fieldImage31').css("opacity", 1);
                //$("option:selected").removeAttr("selected");
            } else {
                $('#fieldImage31').css("opacity", 0.2);
                //$("option:selected").removeAttr("selected");
            }
        }, 250);
    })


});


///team3
//Create new event position
$(document).ready(function() {

    $.getScript('mapster.js', function() {
        $('#fieldImage32').mapster({
            fillOpacity: 0.75,
            fillColor: "#f92c2c",
            stroke: true,
            strokeColor: "red",
            strokeOpacity: 0.8,
            singleSelect: true,
            scaleMap: true,
            mapKey: 'name',
            listKey: 'name',
            onClick: function(e) {
                var keyValue = e.key;
                keyValue = keyValue.replace("location", "");
                // if Asparagus selected, change the tooltip
                keyValue = parseInt(keyValue);
                document.getElementById("sliderPosition32").value = keyValue;
                $("#sliderPosition").val(keyValue);
                Shiny.onInputChange("sliderPosition32", keyValue);

            },
            showToolTip: false,
            toolTipClose: ["tooltip-click", "area-click"],
            areas: [{
                    key: "location1",
                    fillColor: "f92c2c",
                    selected: true
                },
                {
                    key: "location2",
                    fillColor: "f92c2c"
                },
                {
                    key: "location3",
                    fillColor: "f92c2c"
                },
                {
                    key: "location4",
                    fillColor: "f92c2c"
                },
                {
                    key: "location5",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                }, {
                    key: "location6",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location7",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location8",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location9",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location10",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location11",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location12",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location13",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location14",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                }
            ]
        });

    })

    $("#switchtab32").click(function() {
        setTimeout(function() {
            resize();
        }, 250);
    })

    $("#typeselector32").click(function() {
        //alert($("#typeselector31:checked").val());
        setTimeout(function() {
            if ($('input[name=typeselector32]:checked').val() == "free_throw") {
                $('#fieldImage32').css("opacity", 1);
                //$("option:selected").removeAttr("selected");
            } else {
                $('#fieldImage32').css("opacity", 0.2);
                //$("option:selected").removeAttr("selected");
            }
        }, 250);
    })


});
//endteam3

//DIT MOET ALTIJD ONDERAAN!!!
function resize() {
    var maxHeight = 157.85000000000002 //$("#content-body").height()* 0.55;
    var maxWidth = 442.20000000000005 //$("#content-body").width() * 0.55;

    console.log("1image width:")
    console.log(maxWidth)
    console.log("1image height:")
    console.log(maxHeight)

    var image = $('#fieldImage'),
        imgWidth = image.width(),
        imgHeight = image.height(),
        newWidth = 0,
        newHeight = 0;

    console.log("2image width:")
    console.log(image.width())
    console.log("2image height:")
    console.log(image.height())

    if (imgWidth / maxWidth > imgHeight / maxHeight) {
        newWidth = maxWidth;
    } else {
        newHeight = maxHeight;
    }
    image.mapster('resize', newWidth, newHeight, 0);
}


//Percentage position
$(document).ready(function() {

    $.getScript('mapster.js', function() {

        $("#typeselector1").click(function() {
            //alert($("#typeselector:checked").val());
            setTimeout(function() {
                if ($('input[name=typeselector1]:checked').val() == "free_throw") {
                    $('#fieldImage1').mapster({
                        fillOpacity: 0.0
                    });
                    //$("option:selected").removeAttr("selected");
                } else {
                    $('#fieldImage1').mapster({
                        fillOpacity: 0.50,
                        fillColor: "#f92c2c",
                        stroke: true,
                        strokeColor: "red",
                        strokeOpacity: 0.8,
                        singleSelect: true,
                        scaleMap: true,
                        mapKey: 'name',
                        listKey: 'name',
                        onClick: function(e) {
                            var keyValue = e.key;
                            keyValue = keyValue.replace("location", "");
                            // if Asparagus selected, change the tooltip
                            keyValue = parseInt(keyValue);
                            document.getElementById("sliderPosition1").value = keyValue;
                            $("#sliderPosition1").val(keyValue);
                            Shiny.onInputChange("sliderPosition1", keyValue);

                        },
                        showToolTip: false,
                        toolTipClose: ["tooltip-click", "area-click"],
                        areas: [{
                                key: "location1",
                                fillColor: "f92c2c",
                                selected: true
                            },
                            {
                                key: "location2",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location3",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location4",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location5",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            }, {
                                key: "location6",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location7",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location8",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location9",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location10",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location11",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location12",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location13",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            },
                            {
                                key: "location14",
                                strokeColor: "f92c2c",
                                fillColor: "f92c2c"
                            }
                        ]
                    });
                    //$("option:selected").removeAttr("selected");
                }
            }, 250);
        })

        $('#fieldImage1').mapster({
            fillOpacity: 0.50,
            fillColor: "#f92c2c",
            stroke: true,
            strokeColor: "red",
            strokeOpacity: 0.8,
            singleSelect: true,
            scaleMap: true,
            mapKey: 'name',
            listKey: 'name',
            onClick: function(e) {
                var keyValue = e.key;
                keyValue = keyValue.replace("location", "");
                // if Asparagus selected, change the tooltip
                keyValue = parseInt(keyValue);
                document.getElementById("sliderPosition1").value = keyValue;
                $("#sliderPosition1").val(keyValue);
                Shiny.onInputChange("sliderPosition1", keyValue);

            },
            showToolTip: false,
            toolTipClose: ["tooltip-click", "area-click"],
            areas: [{
                    key: "location1",
                    fillColor: "f92c2c",
                    selected: true
                },
                {
                    key: "location2",
                    fillColor: "f92c2c"
                },
                {
                    key: "location3",
                    fillColor: "f92c2c"
                },
                {
                    key: "location4",
                    fillColor: "f92c2c"
                },
                {
                    key: "location5",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                }, {
                    key: "location6",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location7",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location8",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location9",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location10",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location11",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location12",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location13",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                },
                {
                    key: "location14",
                    strokeColor: "f92c2c",
                    fillColor: "f92c2c"
                }
            ]
        });

    })

});

