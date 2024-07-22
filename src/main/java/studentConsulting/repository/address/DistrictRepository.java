package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.address.DistrictEntity;

import java.util.List;
import java.util.Optional;

public interface DistrictRepository extends JpaRepository<DistrictEntity, String> {
    // Tìm quận/huyện theo mã quận/huyện
    Optional<DistrictEntity> findByCode(String code);

    // Tìm các quận/huyện theo mã tỉnh
    List<DistrictEntity> findByProvinceCode(String provinceCode);
}
