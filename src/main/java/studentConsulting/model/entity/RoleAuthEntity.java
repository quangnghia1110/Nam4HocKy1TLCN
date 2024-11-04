package studentConsulting.model.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Objects;

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

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RoleAuthEntity that = (RoleAuthEntity) o;

        return Objects.equals(id, that.id);
    }

}
