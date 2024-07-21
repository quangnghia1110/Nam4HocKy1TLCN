package studentConsulting.model.entity.address;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder 
@Entity
@Table(name = "districts")
@NoArgsConstructor
@AllArgsConstructor
public class DistrictEntity {
    @Id
    @Column(name = "code", nullable = false, length = 20)
    private String code; // Mã quận/huyện

    @Column(name = "name", nullable = false, length = 255)
    private String name; // Tên quận/huyện

    @Column(name = "name_en", nullable = false, length = 255)
    private String nameEn; // Tên quận/huyện tiếng Anh

    @Column(name = "full_name", nullable = false, length = 255)
    private String fullName; // Tên đầy đủ quận/huyện

    @Column(name = "full_name_en", nullable = false, length = 255)
    private String fullNameEn; // Tên đầy đủ quận/huyện tiếng Anh

    @Column(name = "code_name", nullable = false, length = 255)
    private String codeName; // Mã code

    @ManyToOne
    @JoinColumn(name = "province_code", nullable = false, referencedColumnName = "code")
    private ProvinceEntity province; // Mã tỉnh/thành phố

    @OneToMany(mappedBy = "district", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<WardEntity> wards; 

    @OneToMany(mappedBy = "district", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AddressEntity> addresses;
}
