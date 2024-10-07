package studentConsulting.repository.address;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import studentConsulting.model.entity.address.AddressEntity;


public interface AddressRepository extends PagingAndSortingRepository<AddressEntity, Integer>, JpaSpecificationExecutor<AddressEntity>, JpaRepository<AddressEntity, Integer> {

}
