/**
 * Created by Admin on 12/02/2018.
 */
var Bags = {
  isUpdated: false
};

/**
 * Event
 */
Bags.events = function() {
    $(".field-datetimepicker").datetimeEntry({
        spinnerImage: Constants.IMAGES_URL + "spinnerText.png",
        spinnerSize: [30, 20, 8],
        spinnerBigSize: [60, 40, 16],
        datetimeFormat: "Y-O-D H:M:S",
        initialField: 2
    });

    var oneDay = 24 * 60 * 60 * 1000;
    $("#FilterCrFrom").datetimeEntry().change(function () {
        //input cr_from value
        var inputCreatedFromDate = new Date(this.value);
        var inputCreatedFromMonth = inputCreatedFromDate.getMonth() + 1;
        var inputCreatedFromYear = inputCreatedFromDate.getFullYear();

        // input cr_to value
        var inputCreatedToDate = new Date($("#FilterCrTo").val());

        if (inputCreatedFromMonth < 10) {
            inputCreatedFromMonth = '0' + inputCreatedFromMonth
        }

        if ((inputCreatedFromDate.getTime() - inputCreatedToDate.getTime()) < 0) {
            var diffDays = Math.round(Math.abs((inputCreatedFromDate.getTime() - inputCreatedToDate.getTime()) / (oneDay)));

            if (diffDays > 90) {
                var newDateValue = inputCreatedFromYear + '-' + inputCreatedFromMonth + '-' + '01 00:00:00';
                $(this).val(newDateValue);
            }
        }
    });

    $("#FilterCrTo").datetimeEntry().change(function () {
        //input cr_from value
        var inputCreatedFromDate = new Date($("#FilterCrFrom").val());
        var inputCreatedFromMonth = inputCreatedFromDate.getMonth() + 1;
        var inputCreatedFromYear = inputCreatedFromDate.getFullYear();

        // input cr_to value
        var inputCreatedToDate = new Date(this.value);

        if (inputCreatedFromMonth < 10) {
            inputCreatedFromMonth = '0' + inputCreatedFromMonth
        }

        if ((inputCreatedFromDate.getTime() - inputCreatedToDate.getTime()) < 0) {
            var diffDays = Math.round(Math.abs((inputCreatedFromDate.getTime() - inputCreatedToDate.getTime()) / (oneDay)));

            if (diffDays > 90) {
                var newDateValue = inputCreatedFromYear + '-' + inputCreatedFromMonth + '-' + '01 00:00:00';
                $("#FilterCrFrom").val(newDateValue);
            }
        }
    });

    $(".show-package").click(function() {
        console.log($(this).parent().prev());
        $(this).parent().prev().show();
    });

    $(".hide-package").click(function() {
        console.log($(this).parent().prev());
        $(this).parent().prev().hide();
    });

    $(".show_child_bags").on('click',function() {
        if( $(this).text() == 'Show' ) {
            $(this).text('Hide');
        } else {
            $(this).text('Show');
        }
        $(this).parent().parent().parent().find('.list_child_bags').slideToggle();
    });

};

/**
 * Initialize data
 */
Bags.initialize = function () {

};


/**
 * Ready function call
 */
$(document).ready(function () {
    if (window.preventDuplicate) return;
    window.preventDuplicate = 1;

    Bags.initialize();

    Bags.events();

    $("#export-data").click(function() {
        $("#FilterExportBag").val(1)
    });

    $("#export-data-imported").click(function() {
        $("#FilterExportBag").val(1)
    });

    $("#search-bag").click(function() {
        $("#FilterExportBag").val(0)
    });

    $(".station-autocomplete").each(function() {
        $(this).getAllStationsByAutocomplete();
    });

    $(".user-autocomplete").getAllTrucksByAutocomplete();
    $(".truck-autocomplete").getAllTrucksByAutocomplete();
});