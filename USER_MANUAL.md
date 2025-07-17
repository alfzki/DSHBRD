# NusaStat Dashboard - User Manual

## Panduan Pengguna Lengkap

### Daftar Isi
1. [Pengenalan](#pengenalan)
2. [Cara Memulai](#cara-memulai)
3. [Panduan Menu](#panduan-menu)
4. [Tutorial Langkah-demi-Langkah](#tutorial-langkah-demi-langkah)
5. [Interpretasi Hasil](#interpretasi-hasil)
6. [Tips dan Trik](#tips-dan-trik)

## Pengenalan

NusaStat Dashboard adalah aplikasi web interaktif yang dikembangkan untuk analisis statistik data kerentanan sosial Indonesia. Aplikasi ini menyediakan berbagai tools analisis mulai dari statistik deskriptif hingga regresi linear berganda dengan interpretasi otomatis.

### Fitur Utama
- ✅ Analisis data interaktif tanpa coding
- ✅ Visualisasi data yang menarik dan informatif
- ✅ Uji statistik lengkap dengan interpretasi otomatis
- ✅ Download hasil dalam berbagai format
- ✅ Interface yang user-friendly

## Cara Memulai

### Akses Aplikasi
1. Pastikan aplikasi sudah berjalan di: `http://127.0.0.1:3838`
2. Buka browser web (Chrome, Firefox, atau Edge)
3. Navigasi ke alamat tersebut
4. Dashboard akan terbuka dengan halaman Beranda

### Navigasi Dasar
- **Sidebar Kiri**: Menu utama aplikasi
- **Area Utama**: Konten dan input untuk setiap fitur
- **Tombol Download**: Tersedia di setiap halaman untuk export hasil

## Panduan Menu

### 🏠 Beranda
**Tujuan**: Informasi umum dan metadata aplikasi

**Yang Dapat Dilakukan**:
- Membaca overview dashboard
- Melihat informasi dataset
- Memahami metadata variabel
- Mengunduh informasi lengkap (PDF)

**Tips**: Mulai dari sini untuk memahami data yang akan dianalisis

### 🗄️ Manajemen Data
**Tujuan**: Mengubah data kontinu menjadi kategorik

**Langkah-langkah**:
1. **Pilih Variabel**: Dropdown list variabel numerik
2. **Jumlah Kategori**: Input 2-10 kategori
3. **Metode Kategorisasi**:
   - **Interval Sama**: Membagi range nilai menjadi interval yang sama
   - **Kuantil**: Membagi data menjadi grup dengan jumlah observasi sama
4. **Klik "Proses Kategorisasi"**
5. **Lihat hasil** di tabel dan interpretasi

**Output**:
- Tabel dengan variabel asli dan kategori baru
- Interpretasi distribusi kategori
- Download hasil (.csv) dan interpretasi (.txt)

### 📊 Eksplorasi Data
**Tujuan**: Analisis deskriptif dan visualisasi

**Tab 1: Statistik Deskriptif**
1. Pilih variabel dari dropdown
2. Lihat summary statistics (min, max, mean, median, quartiles)
3. Baca interpretasi otomatis

**Tab 2: Visualisasi Grafik**
1. Pilih variabel yang sama
2. Lihat histogram interaktif
3. Gunakan zoom dan hover untuk detail

**Tab 3: Visualisasi Peta**
- Placeholder untuk peta Indonesia (memerlukan data spasial)

**Download Options**:
- Ringkasan statistik (PDF)
- Plot grafik (PNG)

### ✅ Uji Asumsi
**Tujuan**: Menguji asumsi normalitas dan homogenitas

**Tab 1: Uji Normalitas**
1. Pilih variabel numerik
2. Lihat hasil Shapiro-Wilk test
3. Interpretasi: p < 0.05 = tidak normal

**Tab 2: Uji Homogenitas**
1. Pilih variabel dependen (numerik)
2. Pilih variabel grup (kategorik)
3. Lihat hasil Levene's test
4. Interpretasi: p < 0.05 = varians tidak homogen

### 🧮 Statistik Inferensia

#### Uji Beda Rata-Rata
**Satu Sampel**:
1. Pilih "Satu Sampel"
2. Pilih variabel
3. Input nilai μ₀ (rata-rata hipotesis)
4. Pilih hipotesis alternatif
5. Klik "Jalankan Uji"

**Dua Sampel**:
1. Pilih "Dua Sampel"
2. Pilih variabel dependen
3. Pilih variabel grup (harus 2 kategori)
4. Pilih hipotesis alternatif
5. Klik "Jalankan Uji"

#### Uji Proporsi & Varians
**Uji Proporsi**:
1. Pilih "Uji Proporsi"
2. Input jumlah sukses dan total sampel
3. Input proporsi uji (p₀)
4. Pilih hipotesis alternatif
5. Jalankan uji

**Uji Varians**:
1. Pilih "Uji Varians"
2. Pilih variabel
3. Input varians uji (σ₀²)
4. Pilih hipotesis alternatif
5. Jalankan uji

#### ANOVA
**Satu Arah**:
1. Pilih "Satu Arah"
2. Pilih variabel dependen (numerik)
3. Pilih faktor 1 (kategorik, >2 level)
4. Klik "Jalankan ANOVA"

**Dua Arah**:
1. Pilih "Dua Arah"
2. Pilih variabel dependen
3. Pilih faktor 1 dan faktor 2
4. Centang "Sertakan Interaksi" jika diinginkan
5. Jalankan ANOVA

### 📈 Regresi Linear Berganda
**Langkah-langkah**:
1. **Pilih Variabel Dependen**: Satu variabel outcome
2. **Pilih Variabel Independen**: Centang satu atau lebih prediktor
3. **Klik "Jalankan Model"**

**Tab Output**:
- **Hasil Model**: Summary regresi dan persamaan
- **Uji Asumsi**: VIF, Breusch-Pagan, normalitas, plot diagnostik
- **Interpretasi**: Penjelasan lengkap semua hasil

## Tutorial Langkah-demi-Langkah

### Tutorial 1: Analisis Deskriptif Kepadatan Penduduk

1. **Buka Eksplorasi Data**
2. **Pilih variabel**: "kepadatan_penduduk"
3. **Tab Statistik Deskriptif**: Lihat mean, median, range
4. **Tab Visualisasi**: Lihat distribusi histogram
5. **Baca interpretasi**: Apakah distribusi normal atau skewed?
6. **Download**: Simpan ringkasan dan plot

### Tutorial 2: Uji Normalitas Data

1. **Buka Uji Asumsi**
2. **Tab Uji Normalitas**: Pilih "kepadatan_penduduk"
3. **Lihat hasil Shapiro-Wilk**: p-value dan statistik uji
4. **Lihat plot**: Q-Q plot dan histogram dengan kurva normal
5. **Interpretasi**: p < 0.05 → data tidak normal

### Tutorial 3: Membandingkan Rata-rata Antar Provinsi

1. **Buka Statistik Inferensia → Uji Beda Rata-rata**
2. **Pilih "Dua Sampel"**
3. **Variabel dependen**: "sovi_score"
4. **Variabel grup**: "provinsi" (jika tersedia)
5. **Hipotesis**: "Dua arah"
6. **Jalankan uji** dan baca interpretasi

### Tutorial 4: Analisis Regresi Sederhana

1. **Buka Regresi Linear Berganda**
2. **Variabel dependen**: "sovi_score"
3. **Variabel independen**: Centang "kepadatan_penduduk"
4. **Jalankan model**
5. **Tab Hasil Model**: Lihat koefisien dan R-squared
6. **Tab Uji Asumsi**: Periksa VIF dan normalitas residual
7. **Tab Interpretasi**: Baca penjelasan lengkap

### Tutorial 5: Model Regresi Berganda Kompleks

1. **Pilih variabel dependen**: "sovi_score"
2. **Pilih beberapa variabel independen**:
   - "kepadatan_penduduk"
   - "persentase_kemiskinan"
   - "indeks_pendidikan"
3. **Jalankan model**
4. **Evaluasi asumsi**:
   - VIF < 10 (tidak ada multikolinearitas)
   - Breusch-Pagan p > 0.05 (homoskedastisitas)
   - Shapiro-Wilk p > 0.05 (normalitas residual)
5. **Interpretasi koefisien**: Mana yang signifikan?

## Interpretasi Hasil

### P-value dan Signifikansi
- **p < 0.001**: Sangat signifikan (***)
- **p < 0.01**: Signifikan (**)
- **p < 0.05**: Signifikan (*)
- **p ≥ 0.05**: Tidak signifikan

### Uji Hipotesis
- **H₀ (Hipotesis Nol)**: Tidak ada efek/perbedaan
- **H₁ (Hipotesis Alternatif)**: Ada efek/perbedaan
- **Keputusan**: Tolak H₀ jika p < α (biasanya 0.05)

### Regresi Linear
- **R-squared**: % variasi Y yang dijelaskan model (0-100%)
- **Koefisien**: Perubahan Y untuk setiap unit perubahan X
- **p-value koefisien**: Signifikansi pengaruh X terhadap Y

### Interval Kepercayaan
- **95% CI**: Range nilai dengan 95% keyakinan
- **Tidak mengandung 0**: Signifikan
- **Mengandung 0**: Tidak signifikan

## Tips dan Trik

### 💡 Tips Umum
1. **Selalu mulai dengan eksplorasi data** untuk memahami karakteristik dataset
2. **Periksa asumsi** sebelum melakukan uji parametrik
3. **Gunakan interpretasi otomatis** sebagai panduan, tapi selalu pahami konteks
4. **Download hasil** untuk dokumentasi dan presentasi

### 📊 Tips Visualisasi
1. **Hover over plots** untuk melihat nilai detail
2. **Zoom dan pan** pada grafik interaktif
3. **Perhatikan outliers** yang terlihat di histogram dan box plot

### 🧮 Tips Analisis Statistik
1. **Cek ukuran sampel**: Beberapa uji memerlukan minimal n tertentu
2. **Hati-hati multikolinearitas**: VIF > 10 adalah masalah
3. **Normalitas residual** lebih penting daripada normalitas variabel
4. **Interpretasi praktis**: Signifikansi statistik ≠ signifikansi praktis

### 📁 Tips Download
1. **PDF reports** bagus untuk presentasi formal
2. **CSV data** untuk analisis lanjutan di software lain
3. **PNG plots** untuk insert ke dokumen

### ⚠️ Hal yang Perlu Diperhatikan
1. **Data missing**: Dapat mempengaruhi hasil analisis
2. **Outliers ekstrem**: Pertimbangkan transformasi atau removal
3. **Interpretasi kontekstual**: Hasil statistik harus masuk akal secara praktis
4. **Ukuran efek**: Perhatikan magnitude, bukan hanya signifikansi

### 🔧 Troubleshooting Cepat
- **Error loading data**: Refresh halaman atau restart aplikasi
- **Plot tidak muncul**: Pastikan variabel dipilih dengan benar
- **Uji gagal**: Periksa apakah data memenuhi syarat uji
- **Download error**: Pastikan browser mengizinkan download

### 📈 Best Practices
1. **Dokumentasikan workflow** analisis Anda
2. **Backup data** sebelum melakukan transformasi
3. **Validasi hasil** dengan metode alternatif jika memungkinkan
4. **Konsultasi ahli** untuk interpretasi domain-specific

---

**Selamat menggunakan NusaStat Dashboard!** 🎉

Untuk bantuan teknis, lihat file `TROUBLESHOOTING.md` atau konsultasi dengan supervisor/dosen pembimbing.
