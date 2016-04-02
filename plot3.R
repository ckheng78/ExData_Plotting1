require(data.table)
library(data.table)

plot3 <- function() {
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
        
        # generate Plot 3
        # generating to PNG directly
        png(file = "./plot3.png", width = 480, height = 480)
        # Generating line plot for Sub_metering_1 over time
        with(hpc, plot(Sub_metering_1 ~ timestamp, ylab = "Energy sub metering",
                       xlab = "", type = "l", bg = "white"))
        # Add Sub_metering_2 to the same plot
        with(hpc, lines(timestamp, Sub_metering_2, col = "red"))
        # Add Sub_metering_3 to the same plot
        with(hpc, lines(timestamp, Sub_metering_3, col = "blue"))
        # Create legend
        legend("topright",
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
               lty = 1, col = c("black", "red", "blue"))
        # close dev
        dev.off()
}