package studentConsulting.repository.common;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.address.WardEntity;

import java.util.List;
import java.util.Optional;

public interface WardRepository extends JpaRepository<WardEntity, String> {
    Optional<WardEntity> findByCode(String code);

    List<WardEntity> findByDistrictCode(String districtCode);
}
