package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.address.ProvinceEntity;

import java.util.List;
import java.util.Optional;

public interface ProvinceRepository extends JpaRepository<ProvinceEntity, String> {
    // Tìm province theo mã tỉnh
    Optional<ProvinceEntity> findByCode(String code);

    // Tìm tất cả các tỉnh
    List<ProvinceEntity> findAll();
}
