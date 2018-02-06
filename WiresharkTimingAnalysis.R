#
# TITLE: Wireshark Packet Header Analysis
#
# DESCRIPTION
#
#   I want a way to analyze the timing of Wireshark collected data for a specific network
#   command between a device and its driver.  In the example here I'm evaluating the 
#   AWRT response from a device.  I collected the data with Wireshark through a testing
#   sequence and I want to export the packet dissections from the command transmissions
#   and receptions so that I can:
#
#     1. Evaluate the timing between the transmitted request for new data with with AWRT K0 command
#     2. Evaluate the response time of the device between the perception of the transmitted AWRT K0
#        commad and the receipt of the online data from the device
#
# NOTE(S)
#   
#     a. Using packet dissections from WireShark version 2.2.6 (but I don't expect change from different verions)
#     b. Column names in the packet dissections are expected to be: {
#          "No.",
#          "Time",
#          "Source",
#          "Destination",
#          "Protocol",
#          "Length",
#          "Info"}
#     c. I have filtered the packets to provide packet dissections for the transmitted and received commands as:
#          Transmission of "AWRT K0" using "data.data contains 02:20:41:57:52:54:20:4B"
#          Reception of "AWRT e" using "data.data contains 02:20:41:57:52:54:20:30" (expecting error to be zero (0))
#
# PROCESS
#
#     I will import each of the two packet dissection files and then clean up the data found inside
#     so that the two data frames can be joined to evalute the results.
#

library(dplyr)
library(ggplot2)

# Variables
debug <- TRUE

# Input the two packet dissection files from WireShark
xmitPackets <- read.csv(file = './AWRT_Transmit_PacketDissections.csv', sep = ',')
if(debug) head(xmitPackets)
recvPackets <- read.csv(file = './AWRT_Receive_PacketDissections.csv', sep = ',')
if(debug) head(recvPackets)

# Now clean up the data so that they can be joined. I've assumed that each transmit line will match
# to a subsequent response line, so I plan to join the data based on that online 'transaction.' 
# With this resulting table I can calculate the time different (deltaT) between the transmitted
# command and the response from the device
xmitData <- xmitPackets %>%
  mutate(PacketNo = row_number(), XmitTimeMs = Time * 1000) %>%
  rename(XmitSeqNo = 'No.') %>%
  select(PacketNo, XmitSeqNo, XmitTimeMs)
if(debug) head(xmitData)

recvData <- recvPackets %>%
  mutate(PacketNo = row_number(), RecvTimeMs = Time * 1000) %>%
  rename(RecvSeqNo = 'No.') %>%
  select(PacketNo, RecvSeqNo, RecvTimeMs)
if(debug) head(recvData)

transactionData <- xmitData %>%
  inner_join(recvData, by = 'PacketNo') %>%
  mutate(deltaTms = RecvTimeMs - XmitTimeMs) %>%
  select(PacketNo, XmitSeqNo, RecvSeqNo, XmitTimeMs, RecvTimeMs, deltaTms)
if(debug) head(transactionData)

# Evaluation of the timing between successive online command requests
transmitDeltaT <- tail(xmitData, -1) - head(xmitData, -1)
if(debug) head(transmitDeltaT)

m_tdt <- mean(transmitDeltaT$XmitTimeMs)
s_tdt <- sd(transmitDeltaT$XmitTimeMs)
N_tdt <- length(transmitDeltaT$XmitTimeMs)
e_tdt <- qnorm(0.025, lower.tail = FALSE * s_tdt / sqrt(N_tdt))
lower_transmitDeltaT <- m_tdt - e_tdt
upper_transmitDeltaT <- m_tdt + e_tdt
if(debug) {
  cat('\nTranmission DeltaT Results')
  cat('\nMean  = ', m_tdt)
  cat('\nSd    = ', s_tdt)
  cat('\nN     = ', N_tdt)
  cat('\nError = ', e_tdt)
  cat('\nLower = ', lower_transmitDeltaT)
  cat('\nUpper = ', upper_transmitDeltaT)
}

transmitDeltaT %>%
  ggplot(aes(x=XmitTimeMs)) +
  geom_histogram(binwidth = 0.02, color = 'darkgreen', fill = 'lightgreen') + 
  #coord_cartesian(xlim = c(99,101)) + 
  coord_cartesian(xlim = c(lower_transmitDeltaT, upper_transmitDeltaT)) +
  labs(x = '[ms]',
       y = '',
       title = 'Time Difference Distribution',
       subtitle = 'Transmission of Online Commands') + 
  theme_dark()

# Evaluation of the response time from the device to the request of online data
onlineResponseTimes <- transactionData %>%
  select(deltaTms) %>%
  filter(deltaTms <= 5)
if(debug) head(onlineResponseTimes)

m_onl <- mean(onlineResponseTimes$deltaTms)
s_onl <- sd(onlineResponseTimes$deltaTms)
N_onl <- length(onlineResponseTimes$deltaTms)
e_onl <- qnorm(0.025, lower.tail = FALSE * s_onl / sqrt(N_onl))
lower_onlineDeltaT <- m_onl - e_onl
upper_onlineDeltaT <- m_onl + e_onl
if(debug) {
  cat('\nOnline Command Response DeltaT Results')
  cat('\nMean  = ', m_onl)
  cat('\nSd    = ', s_onl)
  cat('\nN     = ', N_onl)
  cat('\nError = ', e_onl)
  cat('\nLower = ', lower_onlineDeltaT)
  cat('\nUpper = ', upper_onlineDeltaT)
}

onlineResponseTimes %>%
  ggplot(aes(x=deltaTms)) +
  geom_histogram(binwidth = 0.01, color = 'darkgreen', fill = 'lightgreen') + 
  coord_cartesian(xlim = c(lower_onlineDeltaT, upper_onlineDeltaT)) + 
  labs(x = '[ms]',
       y = '',
       title = 'Response Time Distribution',
       subtitle = 'Response Time of Online Commands') + 
  theme_dark()

