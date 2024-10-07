package studentConsulting.repository.user;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import studentConsulting.model.entity.user.RoleConsultantEntity;

public interface RoleConsultantRepository extends PagingAndSortingRepository<RoleConsultantEntity, Integer>, JpaSpecificationExecutor<RoleConsultantEntity>, JpaRepository<RoleConsultantEntity, Integer> {

}
