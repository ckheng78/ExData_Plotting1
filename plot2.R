require(data.table)
library(data.table)

plot2 <- function() {
        # check if data file exists in current folder
        if (!file.exists("./household_power_consumption.txt")) {
                stop("household_power_consumption.txt not found!");
        }
        
        # read the file from line 66637 to line 69517
        hpc <- data.table(read.table("./household_power_consumption.txt", 
                                     skip = 66637, nrows = 2880, sep = ";"))
        
        # set column names
        setnames(hpc, names(hpc), c("Date", "Time", "Global_active_power",
                                    "Global_reactive_power", "Voltage",
                                    "Global_intensity", "Sub_metering_1",
                                    "Sub_metering_2", "Sub_metering_3"))
        
        # transform date and time
        hpc[, timestamp := as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]
        
        # generate Plot 2
        # generate to PNG
        png(file = "./plot2.png", width = 480, height = 480)
        # Generating line plot for Global Active Power over time
        with(hpc, plot(Global_active_power ~ timestamp, ylab = "Global Active Power (kilowatts)",
                       xlab = "", type = "l"))
        # close dev
        dev.off()
}