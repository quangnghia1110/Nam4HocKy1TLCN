package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import studentConsulting.model.entity.address.WardEntity;

import java.util.List;
import java.util.Optional;

public interface WardRepository extends PagingAndSortingRepository<WardEntity, String>, JpaSpecificationExecutor<WardEntity>, JpaRepository<WardEntity, String> {
    Optional<WardEntity> findByCode(String code);

    List<WardEntity> findByDistrictCode(String districtCode);

    boolean existsByCode(String code);

    Optional<WardEntity> findByFullName(String fullName);

}
