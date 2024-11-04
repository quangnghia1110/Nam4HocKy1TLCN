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
@Table(name = "address")
@NoArgsConstructor
@AllArgsConstructor
public class AddressEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    @Column(name = "line")
    private String line;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "provinces_id", referencedColumnName = "code")
    @JsonIgnore
    private ProvinceEntity province;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "districts_id", referencedColumnName = "code")
    @JsonIgnore
    private DistrictEntity district;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "wards_id", referencedColumnName = "code")
    @JsonIgnore
    private WardEntity ward;

    @OneToMany(mappedBy = "address", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonIgnore
    private Set<UserInformationEntity> users;

    @Override
    public int hashCode() {
        return Objects.hash(id); // Only use 'id' to prevent infinite recursion
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AddressEntity)) return false;
        AddressEntity that = (AddressEntity) o;
        return Objects.equals(id, that.id); // Use 'id' for equality check
    }
}
