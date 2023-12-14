# Mengimpor pustaka-pustaka yang diperlukan
library(dbscan)
library(ggplot2)
library(geosphere)
library(cluster)
library(geosphere)

# Mengambil sampel 10,000 baris dari data dengan seed yang telah diatur
sampled_data <- read.csv("D:/Data STATBIS ITS/TUGAS AKHIR/preprocessing data/dbscan_klastering.csv")

# Konversi latitude dan longitude ke radian
sampled_data$Latitude.Prediksi <- sampled_data$Latitude.Prediksi * (pi / 180)
sampled_data$Longitude.Prediksi <- sampled_data$Longitude.Prediksi * (pi / 180)



# Menghitung jarak Haversine antara titik-titik
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  return(6371 * c)  # Radius bumi dalam kilometer
}

# Membuat matriks jarak Haversine
dist_matrix <- matrix(NA, nrow = nrow(sampled_data), ncol = nrow(sampled_data))
for (i in 1:nrow(sampled_data)) {
  for (j in 1:nrow(sampled_data)) {
    dist_matrix[i, j] <- haversine_distance(
      sampled_data$Latitude.Prediksi[i],
      sampled_data$Longitude.Prediksi[i],
      sampled_data$Latitude.Prediksi[j],
      sampled_data$Longitude.Prediksi[j]
    )
  }
}

# Menghitung jarak k-nearest neighbors dengan k=30
k_distance <- kNNdist(dist_matrix, k = 75)

# Mengurutkan vektor jarak k-distance
sorted_k_distance <- sort(k_distance)

# Menghasilkan plot k-distance dengan rentang sumbu Y yang sesuai
plot(1:nrow(sampled_data), sorted_k_distance, main = "K-Distance Plot", xlab = "Data Point", ylab = "K-Distance")

# Menambahkan grid pada sumbu x dan y
grid(lty = 2, col = "lightgray")

# Menentukan epsilon (ε) secara eksplisit
elbow <- 150

# Menambahkan garis epsilon pada plot
abline(h = elbow, col = "red", lty = 2)

# Menampilkan nilai "25" di sumbu Y
axis(2, at = c(150), labels = c("150"), col.axis = "red")

# Menampilkan plot
legend("topright", legend = paste("elbow = ", round(elbow, 2)), col = "red", lty = 2)





# Menghitung jarak k-nearest neighbors dengan k=75
k_distance <- kNNdist(dist_matrix, k = 75)

# Mengurutkan vektor jarak k-distance
sorted_k_distance <- sort(k_distance)

# Menghitung turunan kedua plot k-distance
second_derivative <- diff(diff(k_distance))

# Mencari indeks elbow point
elbow_index <- which(second_derivative == min(second_derivative))

# Nilai epsilon (ε) adalah jarak di indeks elbow_index
eps <- sorted_k_distance[elbow_index]

# Menghasilkan plot k-distance
plot(1:nrow(sampled_data), sorted_k_distance, main = "K-Distance Plot", xlab = "Data Point", ylab = "K-Distance")

# Menambahkan garis epsilon pada plot
abline(h = eps, col = "red", lty = 2)

# Menampilkan plot
legend("topright", legend = paste("eps = ", round(eps, 2)), col = "red", lty = 2)


#Didapat nilai parameter optimal untuk metode DBSCAN


#DBSCAN CLUSTERING
dbscan_result <- dbscan(dist_matrix, eps = 150, MinPts = 75)

# Menghitung Silhouette Coefficient
silhouette_score <- silhouette(dbscan_result$cluster, dist_matrix)

# Menampilkan Silhouette Coefficient
print("Silhouette Coefficient:")
print(mean(silhouette_score[, "sil_width"]))

# Menghitung jumlah klaster dan data di dalam masing-masing klaster
num_clusters <- max(dbscan_result$cluster)
cluster_summary <- data.frame(Cluster = 1:num_clusters, Size = 0)
for (i in 1:num_clusters) {
  cluster_summary[i, "Size"] <- sum(dbscan_result$cluster == i)
}

# Menampilkan jumlah klaster dan data di dalam masing-masing klaster
print(cluster_summary)


# Konversi latitude dan longitude dari radian ke derajat sebelum menyimpan
sampled_data$Latitude.Prediksi <- sampled_data$Latitude.Prediksi * (180 / pi)
sampled_data$Longitude.Prediksi <- sampled_data$Longitude.Prediksi * (180 / pi)

# Menambahkan kolom Cluster_Label ke dalam data frame sampel (jika belum ditambahkan)
sampled_data$Cluster_Label <- dbscan_result$cluster

# Menyimpan data frame sampel ke dalam file CSV
write.csv(sampled_data, "dbscan_klastering.csv", row.names = FALSE)



