package studentConsulting.model.entity.address;

import java.sql.Timestamp;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
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
    private Integer id; // Primary key

    @ManyToOne
    @JoinColumn(name = "provinces_id", nullable = false,referencedColumnName = "code")
    private ProvinceEntity province; // Mã tỉnh/thành phố

    @ManyToOne
    @JoinColumn(name = "districts_id",nullable = false, referencedColumnName = "code")
    private DistrictEntity district; // Mã quận/huyện

    @ManyToOne
    @JoinColumn(name = "wards_id", nullable = false,referencedColumnName = "code")
    private WardEntity ward; // Mã xã/phường
    
    @OneToMany(mappedBy = "address", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<UserInformationEntity> users ;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Timestamp createdAt; // Thời gian tạo

    @Column(name = "updated_at", nullable = false)
    private Timestamp updatedAt; // Thời gian cập nhật
}
