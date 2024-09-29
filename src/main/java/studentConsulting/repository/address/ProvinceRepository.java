package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;
import studentConsulting.model.entity.address.ProvinceEntity;

import java.util.List;
import java.util.Optional;

public interface ProvinceRepository extends JpaRepository<ProvinceEntity, String> {
    Optional<ProvinceEntity> findByCode(String code);

    List<ProvinceEntity> findAll();
}
