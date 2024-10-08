package studentConsulting.repository.user;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import studentConsulting.model.entity.user.RoleAskEntity;

import java.util.Optional;

public interface RoleAskRepository extends PagingAndSortingRepository<RoleAskEntity, Integer>, JpaSpecificationExecutor<RoleAskEntity>, JpaRepository<RoleAskEntity, Integer> {
    Optional<RoleAskEntity> findByName(String name);

}
