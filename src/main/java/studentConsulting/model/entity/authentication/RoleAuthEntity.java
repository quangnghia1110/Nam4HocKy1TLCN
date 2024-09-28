package studentConsulting.model.entity.authentication;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

@Data
@Builder
@Entity
@Table(name = "role_auth")
@AllArgsConstructor
@NoArgsConstructor
public class RoleAuthEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "user_id", referencedColumnName = "id")
    @JsonIgnore
    private UserInformationEntity user;

    @Column(name = "token_id")
    private String tokenId;

    @Column(name = "expired_time")
    private Long expiredTime;
}
