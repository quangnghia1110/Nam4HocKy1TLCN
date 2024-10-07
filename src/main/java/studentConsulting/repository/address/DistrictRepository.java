package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import studentConsulting.model.entity.address.DistrictEntity;

import java.util.List;
import java.util.Optional;

public interface DistrictRepository extends PagingAndSortingRepository<DistrictEntity, String>, JpaSpecificationExecutor<DistrictEntity>, JpaRepository<DistrictEntity, String> {
    Optional<DistrictEntity> findByCode(String code);

    List<DistrictEntity> findByProvinceCode(String provinceCode);

    boolean existsByCode(String code);
}
