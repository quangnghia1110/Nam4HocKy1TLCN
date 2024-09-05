package studentConsulting.model.entity.address;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder 
@Entity
@Table(name = "wards")
@NoArgsConstructor
@AllArgsConstructor
public class WardEntity {
    @Id
    @Column(name = "code", nullable = false, length = 20)
    private String code; // Mã xã/phường

    @Column(name = "name", nullable = false, length = 255)
    private String name; // Tên xã/phường

    @Column(name = "name_en", nullable = false, length = 255)
    private String nameEn; // Tên xã/phường tiếng Anh

    @Column(name = "full_name", nullable = false, length = 255)
    private String fullName; // Tên đầy đủ xã/phường

    @Column(name = "full_name_en", nullable = false, length = 255)
    private String fullNameEn; // Tên đầy đủ xã/phường tiếng Anh

    @Column(name = "code_name", nullable = false, length = 255)
    private String codeName; // Mã code

    @ManyToOne
    @JoinColumn(name = "district_code", nullable = false, referencedColumnName = "code")
    @JsonIgnore
    private DistrictEntity district; // Mã quận/huyện

    @OneToMany(mappedBy = "ward", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonIgnore
    private Set<AddressEntity> addresses;
    


}

