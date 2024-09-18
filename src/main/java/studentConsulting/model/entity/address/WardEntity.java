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

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;

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
    @Column(name = "code",  length = 20)
    private String code;

    @Column(name = "name",  length = 255)
    private String name;

    @Column(name = "name_en",  length = 255)
    private String nameEn;

    @Column(name = "full_name",  length = 255)
    private String fullName;

    @Column(name = "full_name_en",  length = 255)
    private String fullNameEn;

    @Column(name = "code_name",  length = 255)
    private String codeName;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "district_code",  referencedColumnName = "code")
    @JsonBackReference
    private DistrictEntity district;

    @OneToMany(mappedBy = "ward", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JsonManagedReference
    private Set<AddressEntity> addresses;
}
