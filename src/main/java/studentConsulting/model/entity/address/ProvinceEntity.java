package studentConsulting.model.entity.address;

import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder 
@Entity
@Table(name = "provinces")
@NoArgsConstructor
@AllArgsConstructor
public class ProvinceEntity {
    @Id
    @Column(name = "code", nullable = false, length = 20)
    private String code; // Mã tỉnh/thành phố

    @Column(name = "name", nullable = false, length = 255)
    private String name; // Tên tỉnh/thành phố

    @Column(name = "name_en", length = 255)
    private String nameEn; // Tên tỉnh/thành phố tiếng Anh

    @Column(name = "full_name", nullable = false, length = 255)
    private String fullName; // Tên đầy đủ tỉnh/thành phố

    @Column(name = "full_name_en", length = 255)
    private String fullNameEn; // Tên đầy đủ tỉnh/thành phố tiếng Anh

    @Column(name = "code_name", length = 255)
    private String codeName; // Mã code
    
    @OneToMany(mappedBy = "province", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<DistrictEntity> districts; 
    
    @OneToMany(mappedBy = "province", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AddressEntity> addresses; 
}

