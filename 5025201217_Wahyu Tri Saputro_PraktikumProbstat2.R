# No. 1
# Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas ???? terhadap
# kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel
# sebanyak 9 responden. Pertama, sebelum melakukan aktivitas ????, peneliti mencatat
# kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut
# diminta melakukan aktivitas ????. Setelah 15 menit, peneliti tersebut mencatat kembali
# kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden
# mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas????.

# Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari
# responden ke-3 ketika belum melakukan aktivitas ???? sebanyak 67, dan setelah
# melakukan aktivitas ???? sebanyak 70.
# a. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel
# diatas
# b. carilah nilai t (p-value)
# c. tentukanlah apakah terdapat pengaruh yang signifikan secara statistika
# dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan
# aktivitas ???? jika diketahui tingkat signifikansi ???? = 5% serta H0 : "tidak ada
# pengaruh yang signifikan secara statistika dalam hal kadar saturasi
# oksigen , sebelum dan sesudah melakukan aktivitas ????" 

#a.
sebelum <- c(78, 75, 67, 77, 70, 72, 28, 74, 77)
setelah <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)

# Cek data
my_data <- data.frame (
  group = rep(c("sebelum", "setelah"), each = 9),
  saturation = c(sebelum, setelah)
)

print(my_data)

#Standar Devisiasi sebelum aktivitas
sd_sebelum <- sd(sebelum)
sd_sebelum

#Standar Devisiasi setelah aktivitas
sd_setelah <- sd(setelah)
sd_setelah

#b.
# Mencari nilai t(p-value) dengan t-test
t.test(sebelum, setelah, alternatif = "greater", var.equal = FALSE)

#c.
var.test(sebelum, setelah)
t.test(sebelum, setelah, mu = 0, alternatif = "two.sided", var.equal = TRUE)


# No. 2
# Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun.
# Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk
# mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata
# 23.500 kilometer dan standar deviasi 3900 kilometer. (Kerjakan menggunakan 
# library seperti referensi pada modul).
# a. Apakah Anda setuju dengan klaim tersebut?
# b. Jelaskan maksud dari output yang dihasilkan!
# c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!

install.packages("BSDA")
library(BSDA)

#a.
print("Setuju")

#b. 
tsum.test(mean.x = 23500, sd(3900), n.x = 100)

#c.
# Jawaban di Repository

# No. 3
# Diketahui perusahaan memiliki seorang data analyst ingin memecahkan
# permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya
# didapatkanlah data berikut dari perusahaan saham tersebut

# Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil
# diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada
# rata-ratanya (??= 0.05)? Buatlah :
# A. H0 dan H1
# B. Hitung Sampel Statistik
# C. Lakukan Uji Statistik (df =2)
# D. Nilai Kritikal
# E. Keputusan
# F. Kesimpulan

#a.
# Jawaban di Repository

#b. 
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19, 
          mean.y = 2.79 , s.y = 1.32, n.y = 27, 
          alternative = "greater", var.equal = TRUE)

#c.
install.packages("mosaic")
library(mosaic)

plotDist(dist = 't', df = 2, col = "blue")

#d.
qchisq(p = 0.05, df = 2, lower.tail = FALSE)

#e.
# Jawaban di Repository

#f.
# Jawaban di Repository

# No. 4
# Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya
# ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
# kucing putih dengan panjangnya masing-masing.
# Jika :
# diketahui dataset https://intip.in/datasetprobstat1
# H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya
# sama
# Maka Kerjakan atau Carilah:
# A. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup 2,
# grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
# lihat apakah ada outlier utama dalam homogenitas varians.
# B. carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
# didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?
# C. Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
# Grup dan beri nama model tersebut model 1.
# D. Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan
# dari H0?
# E. Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p
# yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain? Jelaskan.
# F. Visualisasikan data dengan ggplot2

#a.
myFile  <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt"))
dim(myFile)
head(myFile)
attach(myFile)

myFile$V1 <- as.factor(myFile$V1)
myFile$V1 = factor(myFile$V1,labels = c("Kucing Oren","Kucing Hitam","Kucing Putih","Kucing Oren"))

class(myFile$V1)

group1 <- subset(myFile, V1=="Kucing Persia")
group2 <- subset(myFile, V1=="Kucing Anggora")
group3 <- subset(myFile, V1=="Kucing British Shorthair")

#b.
bartlett.test(Length~V1, data=dataoneway)

#c.
qqnorm(group1$Length)
qqline(group1$Length)

#d.
# Jawaban di Repository

#e.
model1 <- lm(Length~Group, data = myFile)

anova(model1)

TukeyHSD(aov(model1))

#f.
library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")


# No. 5
# Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk
# mengetahui pengaruh suhu operasi (100 ??C, 125 ??C dan 150 ??C) dan tiga jenis kaca
# pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan
# dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil
# Eksperimen. Dengan data tersebut:
# a. Buatlah plot sederhana untuk visualisasi data
# b. Lakukan uji ANOVA dua arah
# c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk
# setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
# d. Lakukan uji Tukey
# e. Gunakan compact letter display untuk menunjukkan perbedaan signifikan
# antara uji Anova dan uji Tukey
# Berikut adalah contoh daftar package dan fungsi yang dapat digunakan (dapat pula
# menggunakan contoh lainnya)
# ??? Packages: readr, ggplot2, multcompView, dplyr
# ??? Function: aov, TukeyHSD, qplot, group_by, summarise, multcompLetters4

install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

#a.
GTL <- read_csv("GTL.csv")
head(GTL)

str(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

#b.
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

#c.
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

#d.
tukey <- TukeyHSD(anova)
print(tukey)

#e.
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)

write.csv("GTL_summary.csv")
