# Penjelasan File `sovi_data.csv` dan `distance.csv`

## `sovi_data.csv`

[cite_start]File ini berisi data kerentanan sosial di Indonesia yang disusun dari Survei Sosial Ekonomi Nasional (SUSENAS) 2017[cite: 124]. Setiap baris dalam file ini mewakili sebuah kabupaten/kota di Indonesia dan berisi variabel-variabel berikut:

* **DISTRICTCODE**: Kode unik untuk setiap kabupaten/kota.
* **CHILDREN**: Persentase populasi anak-anak (di bawah usia 5 tahun).
* **FEMALE**: Persentase populasi perempuan.
* **ELDERLY**: Persentase populasi lansia (di atas 65 tahun).
* **FHEAD**: Persentase rumah tangga yang dikepalai oleh perempuan.
* **FAMILYSIZE**: Rata-rata jumlah anggota dalam satu keluarga.
* **NOELECTRIC**: Persentase rumah tangga yang tidak memiliki akses listrik.
* **LOWEDU**: Persentase populasi dengan tingkat pendidikan rendah (tidak pernah sekolah atau tidak tamat SD).
* **GROWTH**: Laju pertumbuhan penduduk.
* **POVERTY**: Persentase penduduk miskin.
* **ILLITERATE**: Persentase penduduk buta huruf.
* **NOTRAINING**: Persentase angkatan kerja yang tidak pernah mengikuti pelatihan kerja.
* **DPRONE**: Rasio ketergantungan (dependency ratio), yaitu perbandingan antara jumlah penduduk usia non-produktif (di bawah 15 tahun dan di atas 64 tahun) dengan jumlah penduduk usia produktif (15-64 tahun).
* **RENTED**: Persentase rumah tangga yang menyewa rumah.
* **NOSEWER**: Persentase rumah tangga yang tidak memiliki fasilitas sanitasi yang layak.
* **TAPWATER**: Persentase rumah tangga yang tidak memiliki akses air ledeng.
* **POPULATION**: Jumlah total populasi di kabupaten/kota tersebut.

## `distance.csv`

[cite_start]File ini merupakan matriks jarak yang digunakan sebagai informasi tambahan untuk analisis kerentanan sosial, khususnya untuk metode *Fuzzy Geographically Weighted Clustering* (FGWC)[cite: 7].

* **Struktur**: File ini adalah matriks simetris di mana jumlah baris dan kolomnya sama. Setiap baris dan kolom (dilabeli V1, V2, dst.) mewakili sebuah kabupaten/kota di Indonesia.
* **Isi**: Nilai pada perpotongan baris dan kolom dalam matriks ini menunjukkan jarak geografis antara dua kabupaten/kota yang bersangkutan. Sebagai contoh, nilai pada perpotongan baris "V1" dan kolom "V2" adalah jarak antara kabupaten/kota yang diwakili oleh V1 dan V2. Jarak diagonal (misalnya, V1 ke V1) adalah 0.