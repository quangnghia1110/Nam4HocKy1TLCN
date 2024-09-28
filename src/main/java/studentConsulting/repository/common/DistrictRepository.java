package studentConsulting.repository.common;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.address.DistrictEntity;

import java.util.List;
import java.util.Optional;

public interface DistrictRepository extends JpaRepository<DistrictEntity, String> {
    Optional<DistrictEntity> findByCode(String code);

    List<DistrictEntity> findByProvinceCode(String provinceCode);
}
