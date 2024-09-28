package studentConsulting.model.entity.address;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.*;
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
    @JsonBackReference
    private ProvinceEntity province;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "districts_id", referencedColumnName = "code")
    @JsonBackReference
    private DistrictEntity district;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "wards_id", referencedColumnName = "code")
    @JsonBackReference
    private WardEntity ward;

    @OneToMany(mappedBy = "address", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonIgnore
    private Set<UserInformationEntity> users;
}
