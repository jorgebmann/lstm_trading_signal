# Trading Signal generated with LSTM Neural Network

A simple day trading strategy based on time series forecasting with long short-term memory neural networks written in R.

## Installation

```
git clone https://github.com/jorgebmann/lstm_trading_signal
```
A trader workstation account is needed to download historical data and to do automated trading via API.

## Usage

```
R
source('lstm_securities.R')

```

## Documentation

This script generates a trading signal for eurostoxx50 ticker symbols with the following steps:
- downloads 5 years of historical data from trader workstation
- preprocessing including interpolating and removing missing values and calculation of daily returns
- transforming data for sequential processing
- train a lstm neural network with next days return as target
- out-of-sample prediction, sharpe ratio and accuracy calculation of simulated trades


## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)
