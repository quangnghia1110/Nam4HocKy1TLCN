package studentConsulting.model.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Objects;
import java.util.Set;

@Data
@Builder
@Entity
@Table(name = "ward")
@NoArgsConstructor
@AllArgsConstructor
public class WardEntity {
    @Id
    @Column(name = "code", length = 20)
    private String code;

    @Column(name = "name", length = 255)
    private String name;

    @Column(name = "name_en", length = 255)
    private String nameEn;

    @Column(name = "full_name", length = 255)
    private String fullName;

    @Column(name = "full_name_en", length = 255)
    private String fullNameEn;

    @Column(name = "code_name", length = 255)
    private String codeName;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "district_code", referencedColumnName = "code")
    @JsonIgnore
    private DistrictEntity district;

    @OneToMany(mappedBy = "ward", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonIgnore
    private Set<AddressEntity> addresses;

    @Override
    public int hashCode() {
        return Objects.hash(code); // Only use 'code' to prevent infinite recursion
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof WardEntity)) return false;
        WardEntity that = (WardEntity) o;
        return Objects.equals(code, that.code); // Use 'code' for equality check
    }
}
