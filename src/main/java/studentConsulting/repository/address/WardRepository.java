package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.address.WardEntity;

import java.util.List;
import java.util.Optional;

public interface WardRepository extends JpaRepository<WardEntity, String> {
    // Tìm phường/xã theo mã phường/xã
    Optional<WardEntity> findByCode(String code);

    // Tìm các phường/xã theo mã quận/huyện
    List<WardEntity> findByDistrictCode(String districtCode);
}
