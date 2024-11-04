package studentConsulting.repository.admin;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import studentConsulting.model.entity.ProvinceEntity;

import java.util.List;
import java.util.Optional;

public interface ProvinceRepository extends PagingAndSortingRepository<ProvinceEntity, String>, JpaSpecificationExecutor<ProvinceEntity>, JpaRepository<ProvinceEntity, String> {
    Optional<ProvinceEntity> findByCode(String code);

    List<ProvinceEntity> findAll();

    boolean existsByCode(String code);

    Optional<ProvinceEntity> findByFullName(String fullName);

}
