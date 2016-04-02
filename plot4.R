require(data.table)
library(data.table)

plot4 <- function() {
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
        
        # generate Plot 4
        # generating to PNG directly
        png(file = "./plot4.png", width = 480, height = 480)
        # create row-first 2 x 2 plots
        par(mfrow = c(2, 2))
        
        # plot for position (top, left)
        # Generating line plot for Global active power over time
        with(hpc, plot(Global_active_power ~ timestamp, ylab = "Global Active Power",
                       xlab = "", type = "l"))
        
        # plot for position (top, right)
        # Generating line plot for Voltage over time
        with(hpc, plot(Voltage ~ timestamp, xlab = "datetime",
                       type = "l"))
        
        # plot for position (bottom, left)
        # Generating multiple line plots for Sub_metering_1 / 2 / 3 over time
        with(hpc, plot(Sub_metering_1 ~ timestamp, ylab = "Energy sub metering",
                       xlab = "", type = "l", bg = "white"))
        with(hpc, lines(timestamp, Sub_metering_2, col = "red"))
        with(hpc, lines(timestamp, Sub_metering_3, col = "blue"))
        # Generating boxless legend
        legend("topright",
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
               lty = 1, col = c("black", "red", "blue"), bty = "n")
        
        # plot for position (bottom, right)
        # Generating line plot Global reative power over time
        with(hpc, plot(Global_reactive_power ~ timestamp,
                       xlab = "datetime", type = "l"))
        
        # close dev
        dev.off()
}