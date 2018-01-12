#include config.mk

DATA = data
SOIL = $(DATA)/raw_soil_data/
OUT = $(DATA)/processed_soil_data/
CLIMATE = $(DATA)/climate/

## all		: Fetch data and run analysis
.PHONY: all 
all: $(OUT)/daily_station_dat_rainfall.RDS $(OUT)/spring_spot_measurements.RDS

$(OUT)/spring_spot_measurements.RDS: R/spring_spot_measurements.R
	Rscript $<

.PHONY: export
export: $(OUT)/weather_files/ $(OUT)/soil_files/

$(OUT)/soil_files/: R/export_daily_soil_moisture_for_SOILWAT.R $(OUT)/decagon_data_with_station_data.RDS $(CLIMATE)/USSES_climate.csv
	Rscript $<

$(OUT)/weather_files/: R/export_climate_station_data_for_SOILWAT.R $(CLIMATE)/USSES_climate.csv
	Rscript $<

$(OUT)/decagon_data_with_station_data.RDS $(OUT)/daily_station_dat_rainfall.RDS: R/merge_decagon_with_climate_station_data.R $(OUT)/decagon_data_corrected_values.RDS $(CLIMATE)/USSES_climate.csv
	Rscript $<

$(OUT)/decagon_data_corrected_values.RDS: R/correct_decagon_readings.R $(OUT)/decagon_data_corrected_dates.RDS
	Rscript $<

$(OUT)/decagon_data_corrected_dates.RDS: R/correct_decagon_dates.R $(OUT)/decagon_data.RDS
	Rscript $<
	
$(OUT)/decagon_data.RDS: R/import_and_format_decagon_data.R $(DATA)/quad_info.csv $(DATA)/sensor_positions.csv $(SOIL)
	Rscript $<
	
## archive 		: make tar.gz archive of project
.PHONY: archive 
archive: $(ARCHIVE_FILE)

$(ARCHIVE_FILE): $(ARCHIVE_DIR)
	tar -czf $@ $<
	
$(ARCHIVE_DIR): LICENSE README.md Makefile config.mk $(CODE_DIR) $(DATA_DIR)
	mkdir -p $@
	cp -r $^ $@
	touch $@
	
## clean		: Remove temporary files 
.PHONY: clean
clean:
	rm -f Rplots.pdf

## variables	: Print variables.
.PHONY: variables
variables: Makefile 
	@$(foreach V,$(sort $(.VARIABLES)),$(if $(filter-out environment% default automatic,$(origin $V)),$(warning $V=$($V) ($(value $V)))))
	
## help		: Help Menu
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<
