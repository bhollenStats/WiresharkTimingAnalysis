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
# xmitPacketDissectionFilename <- './AWRT_Transmit_PacketDissections.csv'
# recvPacketDissectionFilename <- './AWRT_Receive_PacketDissections.csv'
# xmitPacketDissectionFilename <- './AWRT_Transmit_PacketDissections_Test2.csv'
# recvPacketDissectionFilename <- './AWRT_Receive_PacketDissections_Test2.csv'
xmitPacketDissectionFilename <- './AWRT_Transmit_PacketDissections_ManyTests.csv'
recvPacketDissectionFilename <- './AWRT_Receive_PacketDissections_ManyTests.csv'

# Utility Functions
dataDump <- function(data) {
  m_tdt <- mean(data)
  s_tdt <- sd(data)
  N_tdt <- length(data)
  e_tdt <- qnorm(0.025, lower.tail = FALSE * s_tdt / sqrt(N_tdt))
  lower <- m_tdt - e_tdt
  upper <- m_tdt + e_tdt
  
  cat('\nData Dump Results')
  cat('\n')
  cat('\n  Mean  = ', m_tdt)
  cat('\n  Sd    = ', s_tdt)
  cat('\n  N     = ', N_tdt)
  cat('\n  Error = ', e_tdt)
  cat('\n  Lower = ', lower)
  cat('\n  Upper = ', upper)
  cat('\n  Min   = ', min(data))
  cat('\n  Max   = ', max(data))
}

# Input the two packet dissection files from WireShark
# Expected column names are:
#   [1] "No.","Time","Source","Destination","Protocol","Length","Info"  
xmitPackets <- read.csv(file = xmitPacketDissectionFilename)
if(debug) head(xmitPackets)
recvPackets <- read.csv(file = recvPacketDissectionFilename)
if(debug) head(recvPackets)

# Now clean up the data so that they can be joined. I've assumed that each transmit line will match
# to a subsequent response line, so I plan to join the data based on that online 'transaction.' 
# With this resulting table I can calculate the time different (deltaT) between the transmitted
# command and the response from the device.
#
# xmitData column names should be "PacketNo","XmitSeqNo","XmitTimeMs"
xmitData <- xmitPackets %>%
  mutate(PacketNo = row_number(), XmitTimeMs = Time * 1000) %>%
  rename(XmitSeqNo = 'No.') %>%
  select(PacketNo, XmitSeqNo, XmitTimeMs)
if(debug) head(xmitData)

# recvData column names should be "PacketNo","RecvSeqNo","RecvTimeMs"
recvData <- recvPackets %>%
  mutate(PacketNo = row_number(), RecvTimeMs = Time * 1000) %>%
  rename(RecvSeqNo = 'No.') %>%
  select(PacketNo, RecvSeqNo, RecvTimeMs)
if(debug) head(recvData)

# "joined" transactionData column names should be "PacketNo","XmitSeqNo","RecvSeqNo","XmitTimeMs","RecvTimeMs","deltaTms"
transactionData <- xmitData %>%
  inner_join(recvData, by = 'PacketNo') %>%
  mutate(deltaTms = RecvTimeMs - XmitTimeMs) %>%
  select(PacketNo, XmitSeqNo, RecvSeqNo, XmitTimeMs, RecvTimeMs, deltaTms)
if(debug) head(transactionData)

# Evaluation of the timing between successive online command requests
# transmitDeltaT column names should be "PacketNo","XmitSeqNo","XmitTimeMs"
transmitDeltaT <- tail(xmitData, -1) - head(xmitData, -1)
if(debug) dataDump(transmitDeltaT$XmitTimeMs)

# Filter out some obvious anomalies
transmitDeltaT <- transmitDeltaT %>%
  filter(XmitTimeMs < 200)

transmitDeltaT %>%
  ggplot(aes(x=XmitTimeMs)) +
  geom_histogram(binwidth = 0.1, color = 'darkgreen', fill = 'lightgreen') +
  coord_cartesian(xlim = c(99,101)) +
  labs(x = '[ms]',
       y = '',
       title = 'Time Difference Distribution',
       subtitle = 'Time Difference Between Online Requests in the Driver') +
  theme_dark()

# Evaluation of the response time from the device to the request of online data
# onlineResponseTime column names should be "PacketNo","deltaTms"
onlineResponseTimes <- transactionData %>%
  select(PacketNo, deltaTms) %>%
  filter(deltaTms <= 1000)
if(debug) dataDump(onlineResponseTimes$deltaTms)

onlineResponseTimes %>%
  ggplot(aes(x=deltaTms)) +
  geom_histogram(binwidth = 0.01, color = 'darkgreen', fill = 'lightgreen') +
  coord_cartesian(xlim = c(0.35,0.91)) +
  labs(x = '[ms]',
       y = '',
       title = 'Response Time Distribution',
       subtitle = 'Measured Response Times from Device During Testing') +
  theme_dark()


