library(tidyverse)
library(ggpubr)
library(gridExtra) 

# import
daftar_file <- list.files("./data-raw", pattern="001", full.names = TRUE)
list_data <- map(daftar_file, read_csv)

df <- map_dfc(daftar_file, read_csv)

df2 <- map_dfc(list_data, bind_cols)

df2 <- as_tibble(df2)

identical(df$hari_promosi1,df$hari_promosi2)
df_cln <- df2 %>% 
  select(hari_promosi, contains("jumlah"))

theme_set(theme_pubr())

keuntungan <- ggplot(df_cln,aes(hari_promosi,jumlah_keuntungan)) + geom_line() +
  labs(title="Keuntungan",x="hari promosi", y="jumlah keuntungan")

komplain <- ggplot(df_cln,aes(hari_promosi,jumlah_komplain)) + geom_line() +
  labs(title="komplain",x="hari promosi", y="jumlah komplain")

pengguna <- ggplot(df_cln,aes(hari_promosi,jumlah_pengguna)) + geom_line() +
  labs(title="pengguna",x="hari promosi", y="jumlah pengguna")

grid.arrange(keuntungan, komplain, pengguna,
          ncol=2,nrow=2)

par(mfrow=c(2,2))
# model keuntungan
model_keuntungan = lm(jumlah_keuntungan~hari_promosi, data=df2)
summary(model_keuntungan)
plot(model_keuntungan)

# model komplain
model_komplain = lm(log10(jumlah_komplain)~hari_promosi, data=df2)
summary(model_komplain)
plot(model_komplain)

# model pengguna
model_pengguna = lm(jumlah_pengguna~hari_promosi, data=df2)
summary(model_pengguna)
plot(model_pengguna)

## prediksi keuntungan
predict(model_keuntungan, newdata=data.frame(hari_promosi=(c(130,150,200))), interval="confidence")

predict(model_komplain, newdata=data.frame(hari_promosi=(c(130,150,200))), interval="confidence")^10

predict(model_pengguna, newdata=data.frame(hari_promosi=(c(130,150,200))), interval="confidence") 

