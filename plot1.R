require(data.table)
library(data.table)

plot1 <- function() {
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
        hpc <- transform(hpc, Date = as.Date(Date, format = "%d/%m/%Y"),
                         Time = format(strptime(Time, format = "%H:%M:%S"), format = "%H:%M:%S"))
        
        # generate Plot 1
        # generate to PNG
        png(file = "./plot1.png", width = 480, height = 480)
        # Generating histogram for Global Active Power
        hist(hpc$Global_active_power, xlab = "Global Active Power (kilowatts)",
             main = "Global Active Power", col = "red")
        # close dev
        dev.off()
}