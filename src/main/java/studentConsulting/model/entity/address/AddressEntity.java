package studentConsulting.model.entity.address;

import java.sql.Timestamp;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

@Data
@Builder
@Entity
@Table(name = "address")
@NoArgsConstructor
@AllArgsConstructor
public class AddressEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(nullable = false, name = "id")
    private Integer id;

    @Column(name = "line")
    private String line;

    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt;

    @Column(name = "updated_at", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt;
    
    @ManyToOne
    @JoinColumn(name = "provinces_id", nullable = false, referencedColumnName = "code")
    @JsonIgnore    private ProvinceEntity province; // Mã tỉnh/thành phố

    @ManyToOne
    @JoinColumn(name = "districts_id", nullable = false, referencedColumnName = "code")
    @JsonIgnore
    private DistrictEntity district; // Mã quận/huyện

    @ManyToOne
    @JoinColumn(name = "wards_id", nullable = false, referencedColumnName = "code")
    @JsonIgnore
    private WardEntity ward; // Mã xã/phường
    
    @OneToMany(mappedBy = "address",  fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonIgnore    
    private Set<UserInformationEntity> users;
}
